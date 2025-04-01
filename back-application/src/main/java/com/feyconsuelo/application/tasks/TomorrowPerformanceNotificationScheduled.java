package com.feyconsuelo.application.tasks;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.usecase.performance.GetAllPerformanceImpl;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
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
public class TomorrowPerformanceNotificationScheduled {

    private final FirebaseService firebaseService;
    private final GetAllPerformanceImpl getAllPerformance;

    @Scheduled(cron = "${task.tomorrowPerformanceNotification.schedule}")
    public void tomorrowPerformanceNotification() {

        // obtenemos las actuaciones que tendremos en el dia de mañana
        List<EventResponse> tomorrowPerformances = this.getAllPerformance.execute(
                LocalDate.now().plusDays(1),
                LocalDate.now().plusDays(1),
                Optional.empty(),
                null
        );
        tomorrowPerformances = tomorrowPerformances.stream().filter(event -> Boolean.TRUE.equals(event.getEventPublic())).toList();

        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(tomorrowPerformances))) {

            final StringBuilder message;

            if (tomorrowPerformances.size() > 1) {
                message = new StringBuilder("""
                        Hola,
                        
                        Manaña actuaremos en los siguientes eventos:
                        """);

                for (final EventResponse event : tomorrowPerformances) {
                    message.append("• ").append(event.getTitle()).append("\n");
                }
                message.append("\n");
                message.append("""                        
                        ¡Preparémonos para darlo todo y hacer vibrar al público con nuestra música!
                        
                        A disfrutar!!!
                        """);
            } else {
                message = new StringBuilder(
                        String.format(
                                """
                                        Hola,
                                        
                                        Mañana actuaremos en el siguiente '%s':
                                        • %s
                                        
                                        ¡Preparémonos para darlo todo y hacer vibrar al público con nuestra música!
                                        
                                        A disfrutar!!!
                                        """,
                                tomorrowPerformances.get(0).getPerformanceType().getDescription(),
                                tomorrowPerformances.get(0).getTitle()
                        )
                );
            }

            this.firebaseService.sendNotificationToTopic(
                    "Notificación Actuaciones",
                    message.toString(),
                    NotificationTopicEnum.MUSICO.getTopic()
            );
        }

    }

}
