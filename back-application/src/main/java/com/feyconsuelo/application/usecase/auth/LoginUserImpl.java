package com.feyconsuelo.application.usecase.auth;

import com.feyconsuelo.application.service.security.JwtService;
import com.feyconsuelo.application.service.security.PasswordEncoderService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.auth.AuthRequest;
import com.feyconsuelo.domain.model.auth.AuthResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.auth.LoginUser;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
@Slf4j
public class LoginUserImpl implements LoginUser {

    private final UserService userService;
    private final PasswordEncoderService passwordEncoderService;
    private final JwtService jwtService;

    @Override
    public AuthResponse execute(final AuthRequest authRequest) {
        // 1.- comprobamos si existe el usuario
        final Optional<UserResponse> userOptional = this.userService.get(authRequest.getUsername());

        if (userOptional.isEmpty()) {
            throw new NotFoundException("User not exists");
        }

        // 2.- comprobamos si la contraseña es correcta
        if (Boolean.FALSE.equals(this.passwordEncoderService.matchesPassword(authRequest.getPassword(), userOptional.get().getPassword()))) {
            throw new BadRequestException("User password is incorrect");
        }

        try {
            // 3.- si es valido, entonces generamos el token jwt
            return AuthResponse.builder()
                    .username(userOptional.get().getUsername())
                    .roles(userOptional.get().getRoles())
                    .token(this.jwtService.generateToken(userOptional.get().getUsername(), userOptional.get().getRoles()))
                    .build();
        } catch (final Exception e) {
            log.error("Error generating token", e);
            throw new FeYConsueloException("Error en el proceso de login: " + e.getMessage());
        }
    }
}
