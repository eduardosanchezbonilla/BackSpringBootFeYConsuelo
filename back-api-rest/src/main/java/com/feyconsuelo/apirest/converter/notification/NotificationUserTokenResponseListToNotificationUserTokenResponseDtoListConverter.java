package com.feyconsuelo.apirest.converter.notification;

import com.feyconsuelo.domain.model.notification.NotificationUserTokenResponse;
import com.feyconsuelo.openapi.model.NotificationUserTokenResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class NotificationUserTokenResponseListToNotificationUserTokenResponseDtoListConverter {

    private final NotificationUserTokenResponseToNotificationUserTokenResponseDtoConverter notificationUserTokenResponseToNotificationUserTokenResponseDtoConverter;

    public List<NotificationUserTokenResponseDto> convert(final List<NotificationUserTokenResponse> notificationUserTokenResponseList) {
        if (CollectionUtils.isEmpty(notificationUserTokenResponseList)) {
            return List.of();
        }
        return notificationUserTokenResponseList.stream()
                .map(this.notificationUserTokenResponseToNotificationUserTokenResponseDtoConverter::convert)
                .toList();
    }

}
