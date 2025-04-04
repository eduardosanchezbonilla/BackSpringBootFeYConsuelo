package com.feyconsuelo.application.usecase.notification;

import com.feyconsuelo.application.service.firebase.FirebaseService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.model.notification.NotificationUserTokenResponse;
import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.notification.PurgeUserTokens;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Component
@RequiredArgsConstructor
public class PurgeUserTokensImpl implements PurgeUserTokens {

    private final UserService userService;
    private final FirebaseService firebaseService;

    private String getNamePurge(final UserMusicianResponse user) {
        if (StringUtils.isNotEmpty(user.getUserResponse().getName())) {
            return StringUtils.isEmpty(user.getUserResponse().getSurname()) ?
                    user.getUserResponse().getName() :
                    user.getUserResponse().getName() + " " + user.getUserResponse().getSurname();
        }
        if (user.getMusicianResponse() != null && StringUtils.isNotEmpty(user.getMusicianResponse().getName())) {
            return StringUtils.isEmpty(user.getMusicianResponse().getSurname()) ?
                    user.getMusicianResponse().getName() :
                    user.getMusicianResponse().getName() + " " + user.getMusicianResponse().getSurname();
        }
        return "";
    }

    private String getEmailPurge(final UserMusicianResponse user) {
        if (StringUtils.isNotEmpty(user.getUserResponse().getEmail())) {
            return user.getUserResponse().getEmail();
        }
        if (user.getMusicianResponse() != null && StringUtils.isNotEmpty(user.getMusicianResponse().getEmail())) {
            return user.getMusicianResponse().getEmail();
        }
        return "";
    }

    private String getUserRolePurge(final UserMusicianResponse user) {
        if (user.getUserResponse().getRoles().contains(UserRoleEnum.SUPER_ADMIN.getId())) {
            return UserRoleEnum.SUPER_ADMIN.getId();
        } else if (user.getUserResponse().getRoles().contains(UserRoleEnum.ADMIN.getId())) {
            return UserRoleEnum.ADMIN.getId();
        } else if (user.getUserResponse().getRoles().contains(UserRoleEnum.MUSICO.getId())) {
            return UserRoleEnum.MUSICO.getId();
        } else {
            return UserRoleEnum.INVITADO.getId();
        }
    }

    private Integer getOrderRolePurge(final NotificationUserTokenResponse notificationUserTokenResponse) {
        try {
            return UserRoleEnum.valueOf(notificationUserTokenResponse.getRole()).getOrder();
        } catch (final Exception e) {
            return 200;
        }
    }

    private Integer getVoiceOrderPurge(final NotificationUserTokenResponse notificationUserTokenResponse) {
        try {
            return notificationUserTokenResponse.getVoice().getOrder();
        } catch (final Exception e) {
            return 500000;
        }
    }

    @Override
    public List<NotificationUserTokenResponse> execute() {
        // obtenemos los datos
        final List<UserMusicianResponse> users = this.userService.getAllWithMusicianData();

        if (CollectionUtils.isEmpty(users)) {
            return List.of();
        }

        users.forEach(user -> {
            if (Boolean.FALSE.equals(CollectionUtils.isEmpty(user.getUserResponse().getFirebaseToken()))) {
                // para cada usuario recorremos sus tokens y eliminamos el array los que sean validos, para de esta forma quedarme con los invalidos
                user.getUserResponse().getFirebaseToken().removeIf(token -> Boolean.TRUE.equals(this.firebaseService.checkToken(token)));
            }
        });

        // ahora filtramos solos los que tienen token, y los convertimos al objeto de frespuesta
        return users.stream()
                .filter(user -> user.getUserResponse() != null && Boolean.FALSE.equals(CollectionUtils.isEmpty(user.getUserResponse().getFirebaseToken())))
                .map(user -> NotificationUserTokenResponse.builder()
                        .username(user.getUserResponse().getUsername())
                        .role(this.getUserRolePurge(user))
                        .voice(user.getMusicianResponse() != null ? user.getMusicianResponse().getVoice() : null)
                        .name(this.getNamePurge(user).toUpperCase())
                        .email(this.getEmailPurge(user))
                        .tokens(user.getUserResponse().getFirebaseToken())
                        .build()
                )
                .sorted(
                        Comparator.comparing(this::getOrderRolePurge)
                                .thenComparing(this::getVoiceOrderPurge)
                                .thenComparing(NotificationUserTokenResponse::getName)
                )
                .toList();
    }
}
