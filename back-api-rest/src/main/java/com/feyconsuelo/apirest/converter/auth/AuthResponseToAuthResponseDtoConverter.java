package com.feyconsuelo.apirest.converter.auth;

import com.feyconsuelo.domain.model.auth.AuthResponse;
import com.feyconsuelo.openapi.model.AuthResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class AuthResponseToAuthResponseDtoConverter {

    public AuthResponseDto convert(final AuthResponse authResponse) {
        return AuthResponseDto.builder()
                .username(authResponse.getUsername())
                .roles(authResponse.getRoles())
                .token(authResponse.getToken())
                .build();
    }

}
