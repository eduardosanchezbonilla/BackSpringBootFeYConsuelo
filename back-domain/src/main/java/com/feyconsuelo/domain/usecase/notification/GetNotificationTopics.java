package com.feyconsuelo.domain.usecase.notification;

import com.feyconsuelo.domain.model.notification.NotificationTopicResponse;

import java.util.List;

public interface GetNotificationTopics {

    List<NotificationTopicResponse> execute();

}
