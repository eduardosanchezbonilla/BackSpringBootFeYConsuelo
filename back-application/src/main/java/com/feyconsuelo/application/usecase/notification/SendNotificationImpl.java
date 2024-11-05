package com.feyconsuelo.application.usecase.notification;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.domain.model.notification.NotificationRequest;
import com.feyconsuelo.domain.usecase.notification.SendNotification;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Component
@RequiredArgsConstructor
@Slf4j
public class SendNotificationImpl implements SendNotification {

    private final FirebaseService firebaseService;

    @Override
    public void execute(final NotificationRequest notificationRequest) {

        // enviamos a todos los topics que nos pasen
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(notificationRequest.getTopics()))) {
            for (final String topic : notificationRequest.getTopics()) {
                try {
                    this.firebaseService.sendNotificationToTopic(
                            notificationRequest.getTitle(),
                            notificationRequest.getNotification(),
                            topic
                    );
                } catch (final Exception e) {
                    log.error("Error al enviar notificación a topic: {}", topic, e);
                }
            }
        }

        // enviamos a todos los tokens que nos pasen
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(notificationRequest.getTokens()))) {
            for (final String token : notificationRequest.getTokens()) {
                try {
                    this.firebaseService.sendNotificationToToken(
                            notificationRequest.getTitle(),
                            notificationRequest.getNotification(),
                            token
                    );
                } catch (final Exception e) {
                    log.error("Error al enviar notificación a token: {}", token, e);
                }
            }
        }
    }

}
