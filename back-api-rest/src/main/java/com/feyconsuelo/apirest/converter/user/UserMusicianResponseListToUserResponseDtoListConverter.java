package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.openapi.model.UserGroupByRoleDetailResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserMusicianResponseListToUserResponseDtoListConverter {

    private final UserMusicianResponseToUserResponseDtoConverter userMusicianResponseToUserResponseDtoConverter;
    private final DateService dateService;

    public List<UserGroupByRoleDetailResponseDto> convert(final List<UserMusicianResponse> userMusicianResponseList) {
        if (CollectionUtils.isEmpty(userMusicianResponseList)) {
            return List.of();
        }
        return userMusicianResponseList.stream()
                .map(this.userMusicianResponseToUserResponseDtoConverter::convert)
                .sorted(
                        Comparator.comparing(
                                        (final UserGroupByRoleDetailResponseDto user) -> {
                                            if (Boolean.TRUE.equals(user.getUserDetail().getAppInstalled())) {
                                                if (Boolean.TRUE.equals(user.getUserDetail().getAccessLastHours())) {
                                                    return 1; // Mayor prioridad
                                                } else {
                                                    return 2; // Segunda prioridad
                                                }
                                            } else {
                                                return 3; // Menor prioridad
                                            }
                                        }
                                )
                                .thenComparing(
                                        Comparator.comparing(
                                                (final UserGroupByRoleDetailResponseDto user) -> {
                                                    final String lastAccessDateStr = user.getUserDetail().getLastAccessDate();
                                                    if (StringUtils.isEmpty(lastAccessDateStr)) {
                                                        return 0L;
                                                    } else {
                                                        final LocalDateTime time = this.dateService.stringToDate(lastAccessDateStr, DateTimeFormatter.ISO_DATE_TIME);
                                                        final Instant instant = time.toInstant(ZoneOffset.UTC);
                                                        return instant.toEpochMilli();
                                                    }
                                                },
                                                Comparator.reverseOrder() // Orden descendente
                                        )
                                )
                                /*.thenComparing(
                                        user ->
                                                StringUtils.isEmpty(user.getUserDetail().getLastAccessDate()) ?
                                                        LocalDateTime.now().minusYears(5) // Valor predeterminado para null
                                                        :
                                                        this.dateService.stringToDate(user.getUserDetail().getLastAccessDate(), DateTimeFormatter.ISO_DATE_TIME),
                                        Comparator.reverseOrder() // Orden descendente
                                )*/
                                .thenComparing(UserGroupByRoleDetailResponseDto::getUsername)
                )
                .toList();
    }

}
