package com.feyconsuelo.apirest.converter.notification;

import com.feyconsuelo.domain.model.notification.NotificationTopicResponse;
import com.feyconsuelo.openapi.model.NotificationTopicResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class NotificationTopicResponseListToNotificationTopicResponseDtoListConverter {

    private final NotificationTopicResponseToNotificationTopicResponseDtoConverter notificationTopicResponseToNotificationTopicResponseDtoConverter;

    public List<NotificationTopicResponseDto> convert(final List<NotificationTopicResponse> notificationTopicResponseList) {
        if (CollectionUtils.isEmpty(notificationTopicResponseList)) {
            return List.of();
        }
        return notificationTopicResponseList.stream()
                .map(this.notificationTopicResponseToNotificationTopicResponseDtoConverter::convert)
                .toList();
    }

}
