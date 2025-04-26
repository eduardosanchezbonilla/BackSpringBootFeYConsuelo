package com.feyconsuelo.application.usecase.contactrequest;

import com.feyconsuelo.application.service.contactrequest.ContactRequestService;
import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
import com.feyconsuelo.domain.usecase.contactrequest.InsertContactRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertContactRequestImpl implements InsertContactRequest {

    private static final String NOTIFICATION_TITLE = "Petición de contacto";
    private final FirebaseService firebaseService;
    private final ContactRequestService contactRequestService;

    @Override
    public void execute(final ContactRequest contactRequest) {
        // registramos la solicitud
        this.contactRequestService.insert(contactRequest);

        // enviamos notificacion al super administrador y administrador de que hay un nuevo mensaje en el buzon
        this.firebaseService.sendNotificationToTopic(
                NOTIFICATION_TITLE,
                "El usuario '" + contactRequest.getName() + "' ha enviado una nueva petición de contacto",
                NotificationTopicEnum.SUPER_ADMIN.getTopic()
        );
        this.firebaseService.sendNotificationToTopic(
                NOTIFICATION_TITLE,
                "El usuario '" + contactRequest.getName() + "' ha enviado una nueva petición de contacto",
                NotificationTopicEnum.ADMIN.getTopic()
        );
    }
}
