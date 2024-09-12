package com.feyconsuelo.apirest.converter.auth;

import com.feyconsuelo.domain.model.auth.AuthRequest;
import com.feyconsuelo.openapi.model.AuthRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class AuthRequestDtoToAuthRequestConverter {

    public AuthRequest convert(final AuthRequestDto authRequestDto) {
        return AuthRequest.builder()
                .username(authRequestDto.getUsername().toLowerCase())
                .password(authRequestDto.getPassword())
                .build();
    }

}
