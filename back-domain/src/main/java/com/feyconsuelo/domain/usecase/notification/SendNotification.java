package com.feyconsuelo.domain.usecase.notification;

import com.feyconsuelo.domain.model.notification.NotificationRequest;

public interface SendNotification {

    void execute(NotificationRequest notificationRequest);

}
