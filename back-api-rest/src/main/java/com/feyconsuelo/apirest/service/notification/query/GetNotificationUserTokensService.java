package com.feyconsuelo.apirest.service.notification.query;

import com.feyconsuelo.apirest.converter.notification.NotificationUserTokenResponseListToNotificationUserTokenResponseDtoListConverter;
import com.feyconsuelo.domain.model.notification.NotificationUserTokenResponse;
import com.feyconsuelo.domain.usecase.notification.GetNotificationUserTokens;
import com.feyconsuelo.openapi.model.NotificationUserTokenResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetNotificationUserTokensService {

    private final GetNotificationUserTokens getNotificationUserTokens;

    private final NotificationUserTokenResponseListToNotificationUserTokenResponseDtoListConverter notificationUserTokenResponseListToNotificationUserTokenResponseDtoListConverter;

    public ResponseEntity<List<NotificationUserTokenResponseDto>> getNotificationUserTokens() {
        final List<NotificationUserTokenResponse> notificationUserTokenResponseList = this.getNotificationUserTokens.execute();
        if (CollectionUtils.isEmpty(notificationUserTokenResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.notificationUserTokenResponseListToNotificationUserTokenResponseDtoListConverter.convert(notificationUserTokenResponseList));
    }

}
