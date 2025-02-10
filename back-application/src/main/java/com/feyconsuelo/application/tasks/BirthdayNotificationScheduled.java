package com.feyconsuelo.application.tasks;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
import com.feyconsuelo.domain.model.user.UserResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Slf4j
@Component
@RequiredArgsConstructor
public class BirthdayNotificationScheduled {

    private final MusicianService musicianService;
    private final UserService userService;
    private final FirebaseService firebaseService;

    @Scheduled(cron = "${task.birthdayNotification.schedule}")
    public void birthdayNotification() {

        // cogemos la fecha actual para ver si hay algun musico con cumpleaños en ese dia
        boolean sendNotification = Boolean.FALSE;
        final List<MusicianResponse> musicians = this.musicianService.getByBirthdayDate(LocalDate.now());
        final StringBuilder musiciansBirthDateNames = new StringBuilder();
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musicians))) {
            // enviamos un mensaje a cada musico
            for (final MusicianResponse musician : musicians) {
                // obtenemos el usuario asociado al musico
                final Optional<UserResponse> user = this.userService.get(musician.getDni().toLowerCase(), Boolean.TRUE);

                if (user.isPresent() && Boolean.FALSE.equals(CollectionUtils.isEmpty(user.get().getFirebaseToken()))) {
                    for (final String token : user.get().getFirebaseToken()) {
                        sendNotification = Boolean.TRUE;
                        this.firebaseService.sendNotificationToToken(
                                "Feliz Cumpleaños",
                                "Hola " + musician.getName() + " " + musician.getSurname() + ", desde tu banda Fe y Consuelo, te deseamos lo mejor en el día de tu cumpleaños. \nQue sean muchos años mas junto a nosotros. \nUn fuerte abrazo!!!",
                                token
                        );
                        musiciansBirthDateNames.append("• ").append(musician.getName()).append(" ").append(musician.getSurname()).append("\n");
                    }
                }
            }
        }
        if (Boolean.FALSE.equals(sendNotification)) {
            this.firebaseService.sendNotificationToTopic(
                    "Notificación Cumpleaños",
                    "Hoy no cumple años ninguno de nuestros músicos",
                    NotificationTopicEnum.SUPER_ADMIN.getTopic()
            );
        } else {
            this.firebaseService.sendNotificationToTopic(
                    "Notificacion Cumpleaños",
                    "Hoy han cumplido años los siguientes músicos:\n\n" + musiciansBirthDateNames,
                    NotificationTopicEnum.SUPER_ADMIN.getTopic()
            );
        }
    }

}
