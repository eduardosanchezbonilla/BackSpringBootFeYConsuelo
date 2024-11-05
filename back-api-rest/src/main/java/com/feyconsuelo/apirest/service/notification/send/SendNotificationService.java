package com.feyconsuelo.apirest.service.notification.send;

import com.feyconsuelo.apirest.converter.notification.NotificationRequestDtoToNotificationRequestConverter;
import com.feyconsuelo.domain.usecase.notification.SendNotification;
import com.feyconsuelo.openapi.model.NotificationRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class SendNotificationService {

    private final SendNotification sendNotification;

    private final NotificationRequestDtoToNotificationRequestConverter notificationRequestDtoToNotificationRequestConverter;

    public ResponseEntity<Void> send(final NotificationRequestDto notificationRequestDto) {
        this.sendNotification.execute(
                this.notificationRequestDtoToNotificationRequestConverter.convert(notificationRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
