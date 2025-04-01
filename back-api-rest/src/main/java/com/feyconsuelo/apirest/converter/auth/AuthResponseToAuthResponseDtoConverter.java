package com.feyconsuelo.apirest.converter.auth;

import com.feyconsuelo.apirest.converter.event.EventResponseListToEventResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.apirest.converter.musicianmarchsolo.MusicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter;
import com.feyconsuelo.domain.model.auth.AuthResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.openapi.model.AuthResponseDto;
import com.feyconsuelo.openapi.model.UserDetailResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Slf4j
@Component
@RequiredArgsConstructor
public class AuthResponseToAuthResponseDtoConverter {

    private final MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;
    private final MusicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter musicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter;
    private final EventResponseListToEventResponseDtoListConverter eventResponseListToEventResponseDtoListConverter;

    public AuthResponseDto convert(final AuthResponse authResponse) {
        return AuthResponseDto.builder()
                .username(authResponse.getUsername())
                .roles(authResponse.getRoles())
                .token(authResponse.getToken())
                .musician(authResponse.getMusician() != null ? this.musicianResponseToMusicianResponseDtoConverter.convert(authResponse.getMusician()) : null)
                .musicianMarchSolos(this.musicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter.convert(authResponse.getMusicianMarchSolos()))
                .userDetail(
                        UserDetailResponseDto.builder()
                                .dni(authResponse.getUser().getDni())
                                .name(authResponse.getUser().getName())
                                .surname(authResponse.getUser().getSurname())
                                .direction(authResponse.getUser().getDirection())
                                .municipality(authResponse.getUser().getMunicipality())
                                .province(authResponse.getUser().getProvince())
                                .email(authResponse.getUser().getEmail())
                                .description(authResponse.getUser().getDescription())
                                .image(authResponse.getUser().getImage())
                                .phoneNumber(authResponse.getUser().getPhoneNumber())
                                .build()
                )
                .todayPerformance(
                        CollectionUtils.isEmpty(authResponse.getTodayPerformance()) ?
                                null :
                                authResponse.getTodayPerformance().stream().map(EventResponse::getId).toList()
                )
                //.todayPerformance(this.eventResponseListToEventResponseDtoListConverter.convert(authResponse.getTodayPerformance()))
                .build();
    }

}
