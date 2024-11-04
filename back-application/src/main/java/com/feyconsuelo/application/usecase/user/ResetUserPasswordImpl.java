package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.security.PasswordEncoderService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.user.ResetUserPasswordRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.ResetUserPassword;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class ResetUserPasswordImpl implements ResetUserPassword {

    private final UserService userService;

    private final PasswordEncoderService passwordEncoderService;

    @Override
    public void execute(final ResetUserPasswordRequest resetUserPasswordRequest) {

        final String username = resetUserPasswordRequest.getUsername().toLowerCase();

        final Optional<UserResponse> userOptional = this.userService.get(username);

        // sino existe el usuario devolvemos error de NotFound
        if (userOptional.isEmpty()) {
            throw new NotFoundException("No existe el usuario al que intenta resetear el password");
        }

        // si las passward nueva y repeat son distintas, devolvemos error
        if (Boolean.FALSE.equals(resetUserPasswordRequest.getPassword().equals(resetUserPasswordRequest.getRepeatPassword()))) {
            throw new BadRequestException("La nueva contraseña y la repetición no coinciden");
        }

        // actualizamos
        this.userService.updatePassword(
                username,
                this.passwordEncoderService.encodePassword(resetUserPasswordRequest.getPassword()),
                Boolean.TRUE  // cuando reseteo el password, lo pongo como expirado
        );
    }
}
