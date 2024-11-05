package com.feyconsuelo.apirest.service.notification;

import com.feyconsuelo.apirest.service.notification.query.GetNotificationTopicsService;
import com.feyconsuelo.apirest.service.notification.query.GetNotificationUserTokensService;
import com.feyconsuelo.apirest.service.notification.send.SendNotificationService;
import com.feyconsuelo.openapi.api.NotificationControllerApiDelegate;
import com.feyconsuelo.openapi.model.NotificationRequestDto;
import com.feyconsuelo.openapi.model.NotificationTopicResponseDto;
import com.feyconsuelo.openapi.model.NotificationUserTokenResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class NotificationApiService implements NotificationControllerApiDelegate {

    private final SendNotificationService sendNotificationService;
    private final GetNotificationUserTokensService getNotificationUserTokensService;
    private final GetNotificationTopicsService getNotificationTopicsService;


    @Override
    public ResponseEntity<Void> sendNotification(final NotificationRequestDto notificationRequestDto) {
        return this.sendNotificationService.send(notificationRequestDto);
    }

    @Override
    public ResponseEntity<List<NotificationUserTokenResponseDto>> getNotificationUserTokens() {
        return getNotificationUserTokensService.getNotificationUserTokens();
    }

    @Override
    public ResponseEntity<List<NotificationTopicResponseDto>> getNotificationTopics() {
        return getNotificationTopicsService.getNotificationTopics();
    }


}
