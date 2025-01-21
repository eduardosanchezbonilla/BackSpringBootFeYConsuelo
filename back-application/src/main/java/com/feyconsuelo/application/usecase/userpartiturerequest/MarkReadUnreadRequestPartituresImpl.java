package com.feyconsuelo.application.usecase.userpartiturerequest;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.service.userpartiturerequest.UserPartitureRequestService;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.domain.usecase.userpartiturerequest.MarkReadUnreadRequestPartiture;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Optional;

@Component
@RequiredArgsConstructor
@Slf4j
public class MarkReadUnreadRequestPartituresImpl implements MarkReadUnreadRequestPartiture {

    private static final String NOTIFICATION_TITLE = "Peticion de partituras";
    private final FirebaseService firebaseService;
    private final UserPartitureRequestService userPartitureRequestService;
    private final UserService userService;

    @Override
    public void execute(final UserRequestPartitureRequest userRequestPartitureRequest) {

        // registramos la soliccitud
        this.userPartitureRequestService.markReadUnread(userRequestPartitureRequest);

        // si han enviado notificacion, y el usuario tiene informado el token de firebase, entonces le mandamos la notificacion
        final Optional<UserResponse> userResponse = this.userService.get(userRequestPartitureRequest.getUsername(), Boolean.TRUE);

        if (userResponse.isPresent() && StringUtils.isNotEmpty(userRequestPartitureRequest.getMarkReadUnreadNotificationMessage()) && Boolean.FALSE.equals(CollectionUtils.isEmpty(userResponse.get().getFirebaseToken()))) {
            for (final String token : userResponse.get().getFirebaseToken()) {
                try {
                    this.firebaseService.sendNotificationToToken(
                            NOTIFICATION_TITLE,
                            userRequestPartitureRequest.getMarkReadUnreadNotificationMessage(),
                            token
                    );
                } catch (final Exception e) {
                    log.error("Error al enviar notificación a token: {}", token, e);
                }
            }
        }

    }
}
