package com.feyconsuelo.application.tasks;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.service.utils.SleepService;
import com.feyconsuelo.application.usecase.musicianevent.GetAllMusicianEventsImpl;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
import com.feyconsuelo.domain.model.user.UserResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

@Slf4j
@Component
@RequiredArgsConstructor
public class RehearsalNotificationScheduled {

    private static final Integer DAYS_SEARCH_REHEARSALS = 20;
    private static final Integer DAYS_NOTIFICATION_REHEARSALS = 7;
    private final MusicianService musicianService;
    private final UserService userService;
    private final FirebaseService firebaseService;
    private final GetAllMusicianEventsImpl getAllMusicianEvents;
    private final SleepService sleepService;

    @Scheduled(cron = "${task.rehearsalNotification.schedule}")
    @SuppressWarnings("java:S3776")
    public void rehearsalNotification() {

        // cogemos todos los musicos que tienen firebase token, porque es a los unicos que podemos enviarles notificacion
        final List<MusicianResponse> musicians = this.musicianService.getAll(Boolean.FALSE);
        boolean anyMusicianWithTokenAndDateBefore = Boolean.FALSE;

        final StringBuilder musiciansNegativeNotification = new StringBuilder();
        final StringBuilder musiciansPositiveNotification = new StringBuilder();
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musicians))) {
            for (final MusicianResponse musician : musicians) {
                // obtenemos el usuario asociado al musico
                final Optional<UserResponse> user = this.userService.get(musician.getDni().toLowerCase(), Boolean.TRUE);

                if (user.isPresent() && Boolean.FALSE.equals(CollectionUtils.isEmpty(user.get().getFirebaseToken())) && musician.getDateLastNotificationNonAssistsStreakRehearsals().isBefore(LocalDateTime.now().minusDays(DAYS_NOTIFICATION_REHEARSALS))) {

                    // si tiene token, tendemos que comprobar las condiciones para enviar notificacion y no le hemos enviado notificacion en los ultimos 10 dias
                    anyMusicianWithTokenAndDateBefore = Boolean.TRUE;

                    // 1.- cogemos los ensayos de los ultimos 15 dias
                    final MusicianEventListResponse musicianEventListResponse = this.getAllMusicianEvents.execute(musician.getId(), LocalDate.now().minusDays(DAYS_SEARCH_REHEARSALS), LocalDate.now().minusDays(1), EventTypeEnum.REHEARSAL);

                    // 2.- si en estos 15 dias se han hecho mas de 5 ensayoas comprobamos
                    if (musicianEventListResponse.getEvents().size() >= 5) {
                        // 3.- ordenando por fecha nos debemos quedar con los ultimos 5 ensayos para analizarlos
                        final List<EventResponse> lastFiveRehesals = musicianEventListResponse.getEvents().stream()
                                .sorted(Comparator.comparing(EventResponse::getDate).reversed())
                                .toList()
                                .subList(0, 5);

                        // 3.- si ha faltado a esos ultimos 5 ensayos enviamos notificacion negativa
                        if (Boolean.TRUE.equals(lastFiveRehesals.stream().noneMatch(eventResponse -> eventResponse.getMusicianAssist() != null && eventResponse.getMusicianAssist()))) {
                            for (final String token : user.get().getFirebaseToken()) {
                                this.firebaseService.sendNotificationToToken(
                                        "Ausencia en ensayos",
                                        "Hola " + musician.getName() + " " + musician.getSurname() + ", tus compañeros de 'Fe y Consuelo' te echan de menos ya que no has asistido a los últimos 5 ensayos. \nEsperamos que estés bien. \nNos vemos en el próximo ensayo. \nUn fuerte abrazo!!!",
                                        token
                                );
                                this.sleepService.sleep(200);
                                if (musiciansNegativeNotification.indexOf(musician.getName()) == -1) {
                                    musiciansNegativeNotification.append("• ").append(musician.getName()).append(" ").append(musician.getSurname()).append("\n");

                                    // tenemos que actualizar la fecha de notificacion del musico
                                    this.musicianService.updateLastNotificationNonAssistsStreakRehearsals(musician.getId(), LocalDateTime.now().minusHours(5));
                                }
                            }
                        }

                        // 4.- si ha asistido a todos los ensayos enviamos notificacion positiva
                        if (Boolean.TRUE.equals(lastFiveRehesals.stream().allMatch(eventResponse -> eventResponse.getMusicianAssist() != null && eventResponse.getMusicianAssist()))) {
                            for (final String token : user.get().getFirebaseToken()) {
                                this.firebaseService.sendNotificationToToken(
                                        "Asistencia a ensayos",
                                        "Hola " + musician.getName() + " " + musician.getSurname() + ", desde tu banda Fe y Consuelo, queremos agradecer tu asistencia a los últimos 5 ensayos. \nLa clave del éxito son los músicos comprometidos.\nSigue asi!!! \nUn fuerte abrazo!!!",
                                        token
                                );
                                this.sleepService.sleep(200);
                                if (musiciansPositiveNotification.indexOf(musician.getName()) == -1) {
                                    musiciansPositiveNotification.append("• ").append(musician.getName()).append(" ").append(musician.getSurname()).append("\n");

                                    // tenemos que actualizar la fecha de notificacion del musico
                                    this.musicianService.updateLastNotificationNonAssistsStreakRehearsals(musician.getId(), LocalDateTime.now().minusHours(5));
                                }
                            }
                        }
                    }
                }
            }
        }

        if (Boolean.TRUE.equals(anyMusicianWithTokenAndDateBefore)) {
            if (musiciansNegativeNotification.isEmpty()) {
                this.firebaseService.sendNotificationToTopic(
                        "Notificación Ausencia Ensayos",
                        "No hay ningún musico con racha negativa de ensayos",
                        NotificationTopicEnum.SUPER_ADMIN.getTopic()
                );
                this.sleepService.sleep(200);
            } else {
                this.firebaseService.sendNotificationToTopic(
                        "Notificación Ausencia Ensayos",
                        "Los músicos con racha negativa son:\n\n" + musiciansNegativeNotification,
                        NotificationTopicEnum.SUPER_ADMIN.getTopic()
                );
                this.sleepService.sleep(200);
            }
            if (musiciansPositiveNotification.isEmpty()) {
                this.firebaseService.sendNotificationToTopic(
                        "Notificación Asistencia Ensayos",
                        "No hay ningún musico con racha positiva de ensayos",
                        NotificationTopicEnum.SUPER_ADMIN.getTopic()
                );
                this.sleepService.sleep(200);
            } else {
                this.firebaseService.sendNotificationToTopic(
                        "Notificación Asistencia Ensayos",
                        "Los músicos con racha positiva son:\n\n" + musiciansPositiveNotification,
                        NotificationTopicEnum.SUPER_ADMIN.getTopic()
                );
                this.sleepService.sleep(200);
            }
        } else {
            this.firebaseService.sendNotificationToTopic(
                    "Notificación Ensayos",
                    "No se ha revisado ningún músico, porque no han pasado los días suficientes desde la última notificación",
                    NotificationTopicEnum.SUPER_ADMIN.getTopic()
            );
        }
    }

}
