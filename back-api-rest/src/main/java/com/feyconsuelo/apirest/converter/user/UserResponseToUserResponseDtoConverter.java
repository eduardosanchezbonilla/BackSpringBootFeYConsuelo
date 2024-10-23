package com.feyconsuelo.apirest.converter.user;

import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.openapi.model.UserResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserResponseToUserResponseDtoConverter {

    public UserResponseDto convert(final UserResponse userResponse) {
        return UserResponseDto.builder()
                .username(userResponse.getUsername())
                .password(userResponse.getPassword())
                .roles(userResponse.getRoles())
                .passwordExpired(userResponse.getPasswordExpired())
                .dni(userResponse.getDni())
                .name(userResponse.getName())
                .surname(userResponse.getSurname())
                .direction(userResponse.getDirection())
                .municipality(userResponse.getMunicipality())
                .province(userResponse.getProvince())
                .email(userResponse.getEmail())
                .description(userResponse.getDescription())
                .image(userResponse.getImage())
                .build();
    }

}
