package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserMusicianResponse;
import com.feyconsuelo.openapi.model.UserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserMusicianResponseToUserResponseDtoConverter {

    public UserResponseDto convert(final UserMusicianResponse userMusicianResponse) {
        return UserResponseDto.builder()
                .username(userMusicianResponse.getUserResponse().getUsername())
                .password(userMusicianResponse.getUserResponse().getPassword())
                .roles(userMusicianResponse.getUserResponse().getRoles())
                .passwordExpired(userMusicianResponse.getUserResponse().getPasswordExpired())
                .dni(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getDni() : userMusicianResponse.getMusicianResponse().getDni())
                .name(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getName() : userMusicianResponse.getMusicianResponse().getName())
                .surname(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getSurname() : userMusicianResponse.getMusicianResponse().getSurname())
                .direction(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getDirection() : userMusicianResponse.getMusicianResponse().getDirection())
                .municipality(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getMunicipality() : userMusicianResponse.getMusicianResponse().getMunicipality())
                .province(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getProvince() : userMusicianResponse.getMusicianResponse().getProvince())
                .email(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getEmail() : userMusicianResponse.getMusicianResponse().getEmail())
                .description(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getDescription() : "")
                .image(userMusicianResponse.getMusicianResponse() == null ? userMusicianResponse.getUserResponse().getImage() : userMusicianResponse.getMusicianResponse().getImage())
                .build();
    }

}
