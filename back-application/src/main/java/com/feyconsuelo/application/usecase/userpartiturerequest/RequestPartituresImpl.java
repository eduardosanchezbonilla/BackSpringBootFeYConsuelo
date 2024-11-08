package com.feyconsuelo.application.usecase.userpartiturerequest;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.service.userpartiturerequest.UserPartitureRequestService;
import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.domain.usecase.userpartiturerequest.RequestPartiture;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class RequestPartituresImpl implements RequestPartiture {

    private static final String NOTIFICATION_TOPIC = NotificationTopicEnum.SUPER_ADMIN.getTopic();
    private static final String NOTIFICATION_TITLE = "Peticion de partituras";
    private final FirebaseService firebaseService;
    private final UserPartitureRequestService userPartitureRequestService;

    @Override
    public void execute(final UserRequestPartitureRequest userRequestPartitureRequest) {

        // registramos la soliccitud
        this.userPartitureRequestService.insert(userRequestPartitureRequest);

        // enviamos notificacion al super administrador que es el unico que puede asignar partituras
        this.firebaseService.sendNotificationToTopic(
                NOTIFICATION_TITLE,
                "El usuario '" + userRequestPartitureRequest.getUsername() + "' ha solicitado partituras",
                NOTIFICATION_TOPIC
        );
    }
}
