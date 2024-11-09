package com.feyconsuelo.domain.usecase.notification;

import com.feyconsuelo.domain.model.notification.NotificationUserTokenResponse;

import java.util.List;

public interface PurgeUserTokens {

    List<NotificationUserTokenResponse> execute();

}
