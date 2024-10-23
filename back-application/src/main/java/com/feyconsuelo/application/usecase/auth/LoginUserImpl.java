package com.feyconsuelo.application.usecase.auth;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.security.JwtService;
import com.feyconsuelo.application.service.security.PasswordEncoderService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.exception.NotAuthorizedException;
import com.feyconsuelo.domain.exception.PasswordExpiredException;
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
    private final MusicianService musicianService;

    @Override
    public AuthResponse execute(final AuthRequest authRequest) {
        // 1.- comprobamos si existe el usuario
        final Optional<UserResponse> userOptional = this.userService.get(authRequest.getUsername());

        if (userOptional.isEmpty()) {
            throw new NotAuthorizedException("No existe el usuario introducido");
        }

        // 2.- comprobamos si la contraseña es correcta
        if (Boolean.FALSE.equals(this.passwordEncoderService.matchesPassword(authRequest.getPassword(), userOptional.get().getPassword()))) {
            throw new NotAuthorizedException("El password introducido es incorrecto");
        }

        // 3.- comprobamos si el password ha expirado
        if (Boolean.TRUE.equals(userOptional.get().getPasswordExpired())) {
            throw new PasswordExpiredException("El password ha expirado, debe cambiarlo");
        }

        try {
            // 4.- si es valido, entonces generamos el token jwt
            return AuthResponse.builder()
                    .username(userOptional.get().getUsername())
                    .roles(userOptional.get().getRoles())
                    .token(this.jwtService.generateToken(userOptional.get().getUsername(), userOptional.get().getRoles()))
                    .musician(this.musicianService.getByDni(userOptional.get().getUsername().toUpperCase()).orElse(null))
                    .user(userOptional.get())
                    .build();
        } catch (final Exception e) {
            log.error("Error en la generacion del token de autenticación", e);
            throw new FeYConsueloException("Error en el proceso de login: " + e.getMessage());
        }
    }
}
