package com.feyconsuelo.apirest.converter.notification;

import com.feyconsuelo.domain.model.notification.NotificationUserTokenResponse;
import com.feyconsuelo.openapi.model.NotificationUserTokenResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class NotificationUserTokenResponseToNotificationUserTokenResponseDtoConverter {

    public NotificationUserTokenResponseDto convert(final NotificationUserTokenResponse notificationUserTokenResponse) {
        return NotificationUserTokenResponseDto.builder()
                .username(notificationUserTokenResponse.getUsername())
                .name(notificationUserTokenResponse.getName())
                .email(notificationUserTokenResponse.getEmail())
                .tokens(notificationUserTokenResponse.getTokens())
                .build();
    }

}
