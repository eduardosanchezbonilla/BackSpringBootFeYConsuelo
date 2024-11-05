package com.feyconsuelo.apirest.converter.notification;

import com.feyconsuelo.domain.model.notification.NotificationRequest;
import com.feyconsuelo.openapi.model.NotificationRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class NotificationRequestDtoToNotificationRequestConverter {

    public NotificationRequest convert(final NotificationRequestDto notificationRequestDto) {
        return NotificationRequest.builder()
                .title(notificationRequestDto.getTitle())
                .notification(notificationRequestDto.getNotification())
                .topics(notificationRequestDto.getTopics())
                .tokens(notificationRequestDto.getTokens())
                .build();
    }

}
