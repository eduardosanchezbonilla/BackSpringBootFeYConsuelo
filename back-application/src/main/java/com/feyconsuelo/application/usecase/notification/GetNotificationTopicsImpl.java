package com.feyconsuelo.application.usecase.notification;

import com.feyconsuelo.domain.model.notification.NotificationTopicEnum;
import com.feyconsuelo.domain.model.notification.NotificationTopicResponse;
import com.feyconsuelo.domain.usecase.notification.GetNotificationTopics;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Stream;

@Component
@RequiredArgsConstructor
public class GetNotificationTopicsImpl implements GetNotificationTopics {

    @Override
    public List<NotificationTopicResponse> execute() {
        return Stream.of(NotificationTopicEnum.values())
                .map(role -> NotificationTopicResponse.builder()
                        .topic(role.getTopic())
                        .topicName(role.getTopicName())
                        .build()
                )
                .toList();
    }
}
