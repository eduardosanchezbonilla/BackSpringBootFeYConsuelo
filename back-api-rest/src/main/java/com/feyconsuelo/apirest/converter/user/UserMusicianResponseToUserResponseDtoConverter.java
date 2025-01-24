package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.openapi.model.UserDetailResponseDto;
import com.feyconsuelo.openapi.model.UserGroupByRoleDetailResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserMusicianResponseToUserResponseDtoConverter {

    private static final Integer LAST_ACCESS_HOURS = 10;
    private final DateService dateService;


    private Boolean getAppInstalled(final LocalDateTime lastAccessDate) {
        return lastAccessDate != null;
    }

    private Boolean getAccessLastHours(final LocalDateTime lastAccessDate) {
        return lastAccessDate != null && lastAccessDate.isAfter(LocalDateTime.now().minusHours(LAST_ACCESS_HOURS));
    }

    public UserGroupByRoleDetailResponseDto convert(final UserMusicianResponse userMusicianResponse) {
        return UserGroupByRoleDetailResponseDto.builder()
                .username(userMusicianResponse.getUserResponse().getUsername())
                .password(userMusicianResponse.getUserResponse().getPassword())
                .roles(userMusicianResponse.getUserResponse().getRoles())
                .passwordExpired(userMusicianResponse.getUserResponse().getPasswordExpired())
                .userDetail(
                        UserDetailResponseDto.builder()
                                .dni(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getDni() : userMusicianResponse.getMusicianResponse().getDni())
                                .name(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getName() : userMusicianResponse.getMusicianResponse().getName())
                                .surname(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getSurname() : userMusicianResponse.getMusicianResponse().getSurname())
                                .direction(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getDirection() : userMusicianResponse.getMusicianResponse().getDirection())
                                .municipality(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getMunicipality() : userMusicianResponse.getMusicianResponse().getMunicipality())
                                .province(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getProvince() : userMusicianResponse.getMusicianResponse().getProvince())
                                .email(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getEmail() : userMusicianResponse.getMusicianResponse().getEmail())
                                .description(userMusicianResponse.getUserResponse().getDescription())
                                .image(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getImage() : userMusicianResponse.getMusicianResponse().getImage())
                                .phoneNumber(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getPhoneNumber() : userMusicianResponse.getMusicianResponse().getPhoneNumber())
                                .lastAccessDate(this.dateService.dateToString(userMusicianResponse.getUserResponse().getLastAccessDate(), DateTimeFormatter.ISO_DATE_TIME))
                                .appInstalled(this.getAppInstalled(userMusicianResponse.getUserResponse().getLastAccessDate()))
                                .accessLastHours(this.getAccessLastHours(userMusicianResponse.getUserResponse().getLastAccessDate()))
                                .build()
                )
                .build();
    }

}
