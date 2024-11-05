package com.feyconsuelo.apirest.service.notification.query;

import com.feyconsuelo.apirest.converter.notification.NotificationTopicResponseListToNotificationTopicResponseDtoListConverter;
import com.feyconsuelo.domain.model.notification.NotificationTopicResponse;
import com.feyconsuelo.domain.usecase.notification.GetNotificationTopics;
import com.feyconsuelo.openapi.model.NotificationTopicResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetNotificationTopicsService {

    private final GetNotificationTopics getNotificationTopics;

    private final NotificationTopicResponseListToNotificationTopicResponseDtoListConverter notificationTopicResponseListToNotificationTopicResponseDtoListConverter;

    public ResponseEntity<List<NotificationTopicResponseDto>> getNotificationTopics() {
        final List<NotificationTopicResponse> notificationTopicResponseList = this.getNotificationTopics.execute();
        if (CollectionUtils.isEmpty(notificationTopicResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.notificationTopicResponseListToNotificationTopicResponseDtoListConverter.convert(notificationTopicResponseList));
    }

}
