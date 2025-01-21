package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.security.PasswordEncoderService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.user.UpdateUserPasswordRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.UpdateUserPassword;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateUserPasswordImpl implements UpdateUserPassword {

    private final UserService userService;

    private final PasswordEncoderService passwordEncoderService;

    @Override
    public void execute(final UpdateUserPasswordRequest updateUserPasswordRequest) {

        final Optional<UserResponse> userOptional = this.userService.get(updateUserPasswordRequest.getUsername(), Boolean.TRUE);

        // sino existe el usuario devolvemos error de NotFound
        if (userOptional.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta modificar");
        }

        // si la contrasena actual no coincide con la almacenada devolvemos error de BadRequest
        if (Boolean.FALSE.equals(this.passwordEncoderService.matchesPassword(updateUserPasswordRequest.getCurrentPassword(), userOptional.get().getPassword()))) {
            throw new BadRequestException("La contraseña actual no coincide con la almacenada");
        }

        // si las passward nueva y repeat son distintas, devolvemos error
        if (Boolean.FALSE.equals(updateUserPasswordRequest.getNewPassword().equals(updateUserPasswordRequest.getRepeatNewPassword()))) {
            throw new BadRequestException("La nueva contraseña y la repetición no coinciden");
        }

        // el nuevo password no puede ser igual al actual
        if (Boolean.TRUE.equals(this.passwordEncoderService.matchesPassword(updateUserPasswordRequest.getNewPassword(), userOptional.get().getPassword()))) {
            throw new BadRequestException("La nueva contraseña no puede ser igual a la actual");
        }

        // actualizamos
        this.userService.updatePassword(
                updateUserPasswordRequest.getUsername(),
                this.passwordEncoderService.encodePassword(updateUserPasswordRequest.getNewPassword()),
                Boolean.FALSE  // cuando actualizo el password, lo pongo como no expirado
        );
    }
}
