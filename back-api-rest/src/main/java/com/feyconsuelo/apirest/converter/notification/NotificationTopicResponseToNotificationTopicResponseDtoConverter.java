package com.feyconsuelo.apirest.converter.notification;

import com.feyconsuelo.domain.model.notification.NotificationTopicResponse;
import com.feyconsuelo.openapi.model.NotificationTopicResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class NotificationTopicResponseToNotificationTopicResponseDtoConverter {

    public NotificationTopicResponseDto convert(final NotificationTopicResponse notificationTopicResponse) {
        return NotificationTopicResponseDto.builder()
                .topic(notificationTopicResponse.getTopic())
                .topicName(notificationTopicResponse.getTopicName())
                .build();
    }

}
