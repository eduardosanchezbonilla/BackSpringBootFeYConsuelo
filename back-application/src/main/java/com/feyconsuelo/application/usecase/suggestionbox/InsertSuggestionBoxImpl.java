package com.feyconsuelo.application.usecase.suggestionbox;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.service.suggestionbox.SuggestionBoxService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
import com.feyconsuelo.domain.model.suggestionbox.SuggestionBoxRequest;
import com.feyconsuelo.domain.usecase.suggestionbox.InsertSuggestionBox;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertSuggestionBoxImpl implements InsertSuggestionBox {

    private static final String NOTIFICATION_TITLE = "Buzón de sugerencias";
    private final TokenInfoExtractorService tokenInfoExtractorService;
    private final FirebaseService firebaseService;
    private final SuggestionBoxService suggestionBoxService;

    @Override
    public void execute(final SuggestionBoxRequest suggestionBoxRequest) {
        // registramos la solicitud
        this.suggestionBoxService.insert(suggestionBoxRequest);

        // enviamos notificacion al super administrador y administrador de que hay un nuevo mensaje en el buzon
        this.firebaseService.sendNotificationToTopic(
                NOTIFICATION_TITLE,
                "El usuario '" + this.tokenInfoExtractorService.getUsername() + "' ha enviado un nuevo mensaje al buzón",
                NotificationTopicEnum.SUPER_ADMIN.getTopic()
        );
        this.firebaseService.sendNotificationToTopic(
                NOTIFICATION_TITLE,
                "Ha llegado un nuevo mensaje al buzón de sugerencias",
                NotificationTopicEnum.ADMIN.getTopic()
        );
    }
}
