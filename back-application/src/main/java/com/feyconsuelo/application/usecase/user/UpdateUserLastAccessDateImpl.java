package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.UpdateUserLastAccessDate;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateUserLastAccessDateImpl implements UpdateUserLastAccessDate {

    private static final String NOTIFICATION_TOPIC = NotificationTopicEnum.SUPER_ADMIN.getTopic();
    private static final String NOTIFICATION_TITLE = "Instalacion aplicacion";
    private final UserService userService;
    private final FirebaseService firebaseService;

    @Override
    public void execute(final String username) {

        final Optional<UserResponse> userOptional = this.userService.get(username.toLowerCase(), Boolean.TRUE);

        // sino existe el usuario devolvemos error de NotFound
        if (userOptional.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta modificar");
        }

        // si existe el usuario miramos si ya tiene lassAccessDate, porque sino es asi, es el primer acceso y enviaremos notificacion al super administrador
        if (userOptional.get().getLastAccessDate() == null) {
            // enviar notificacion al administrador
            this.firebaseService.sendNotificationToTopic(
                    NOTIFICATION_TITLE,
                    "El usuario '" + username + " (" + userOptional.get().getName() + " " + userOptional.get().getSurname() + ")' ha realizado su primera acceso a la aplicacion",
                    NOTIFICATION_TOPIC
            );
        }

        this.userService.updateLastAccessDate(username.toLowerCase(), LocalDateTime.now());
    }
}