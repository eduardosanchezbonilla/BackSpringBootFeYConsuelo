package com.feyconsuelo.apirest.service.auth;

import com.feyconsuelo.apirest.converter.auth.AuthRequestDtoToAuthRequestConverter;
import com.feyconsuelo.apirest.converter.auth.AuthResponseToAuthResponseDtoConverter;
import com.feyconsuelo.domain.usecase.auth.LoginUser;
import com.feyconsuelo.openapi.api.AuthControllerApiDelegate;
import com.feyconsuelo.openapi.model.AuthRequestDto;
import com.feyconsuelo.openapi.model.AuthResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthApiService implements AuthControllerApiDelegate {

    private final LoginUser loginUser;
    private final AuthResponseToAuthResponseDtoConverter authResponseToAuthResponseDtoConverter;
    private final AuthRequestDtoToAuthRequestConverter authRequestDtoToAuthRequestConverter;

    @Override
    public ResponseEntity<AuthResponseDto> loginUser(final AuthRequestDto authRequestDto) {
        try {
            final var authResponse = this.loginUser.execute(this.authRequestDtoToAuthRequestConverter.convert(authRequestDto));

            if (authResponse == null || Boolean.TRUE.equals(StringUtils.isEmpty(authResponse.getToken()))) {
                return ResponseEntity.noContent().build();
            } else {
                return ResponseEntity.ok(this.authResponseToAuthResponseDtoConverter.convert(authResponse));
            }
        } catch (final Exception e) {
            log.error("Error al logar al usuario: {}", e.getMessage(), e);
            throw e;
        }
    }

}
