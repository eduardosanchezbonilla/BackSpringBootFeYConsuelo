package com.feyconsuelo.infrastructure.service.firebase;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.google.firebase.messaging.AndroidConfig;
import com.google.firebase.messaging.AndroidNotification;
import com.google.firebase.messaging.FirebaseMessaging;
import com.google.firebase.messaging.Message;
import com.google.firebase.messaging.Notification;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class FirebaseServiceImpl implements FirebaseService {

    private static final Integer NOTIFICATION_TTL = 86400 * 1000; // 1 día
    private static final AndroidConfig.Priority NOTIFICATION_PRIORITY = AndroidConfig.Priority.HIGH;
    private static final String NOTIFICATION_ICON = "logo_notification"; // Nombre del icono sin la extensión
    private static final String NOTIFICATION_COLOR_ICON = "#FF0000"; // Color opcional del ícono
    private final FirebaseMessaging firebaseMessaging;

    @SuppressWarnings("java:S125")
    /* IOS
    ApnsConfig apnsConfig = ApnsConfig.builder()
            .setAps(Aps.builder().setAlert("Título para iOS").build())
            .build();

    Message msg = Message.builder()
            .setTopic("admin")
            .setApnsConfig(apnsConfig)
            .setNotification(new Notification("Título", "Cuerpo del mensaje"))
            .putData("body", "some data")
            .build();
     */

    private AndroidConfig getConfigAndroid() {
        return AndroidConfig.builder()
                .setTtl(NOTIFICATION_TTL)  // 1 día
                .setPriority(NOTIFICATION_PRIORITY)
                .setCollapseKey(null)
                .setNotification(
                        AndroidNotification.builder()
                                .setIcon(NOTIFICATION_ICON)
                                .setColor(NOTIFICATION_COLOR_ICON)
                                .build()
                )
                .build();
    }

    @Override
    @Async
    public void sendNotificationToTopic(
            final String title,
            final String notification,
            final String topic
    ) {
        try {
            final Message msg = Message.builder()
                    .setTopic(topic)
                    .setNotification(
                            Notification.builder()
                                    .setTitle(title)
                                    .setBody(notification)
                                    .build()
                    )
                    .putData("title", title)
                    .putData("body", notification)
                    .setAndroidConfig(this.getConfigAndroid())
                    .build();

            this.firebaseMessaging.send(msg);
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al enviar la notificacion al topic '" + topic + "':" + e.getMessage());
        }
    }

    @Override
    @Async
    public void sendNotificationToToken(
            final String title,
            final String notification,
            final String token
    ) {
        try {
            final Message msg = Message.builder()
                    .setToken(token)
                    .setNotification(
                            Notification.builder()
                                    .setTitle(title)
                                    .setBody(notification)
                                    .build()
                    )
                    .putData("title", title)
                    .putData("body", notification)
                    .setAndroidConfig(this.getConfigAndroid())
                    .build();

            this.firebaseMessaging.send(msg);
        } catch (final Exception e) {
            throw new FeYConsueloException("Error al enviar la notificacion al token '" + token + "':" + e.getMessage());
        }
    }

    @Override
    public Boolean checkToken(final String token) {
        try {
            // este mensaje no llegaria nunca, solo sirve para ver si existe el token o no
            final Message msg = Message.builder()
                    .setToken(token)
                    .build();
            this.firebaseMessaging.send(msg);
            return Boolean.TRUE;
        } catch (final Exception e) {
            return Boolean.FALSE;
        }
    }

}
