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

        final Optional<UserResponse> userOptional = this.userService.get(updateUserPasswordRequest.getUsername());

        // sino existe el usuario devolvemos error de NotFound
        if (userOptional.isEmpty()) {
            throw new NotFoundException("User to update not found");
        }

        // si la contrasena actual no coincide con la almacenada devolvemos error de BadRequest
        if (Boolean.FALSE.equals(this.passwordEncoderService.matchesPassword(updateUserPasswordRequest.getCurrentPassword(), userOptional.get().getPassword()))) {
            throw new BadRequestException("Current password does not match");
        }

        // si las passward nueva y repeat son distintas, devolvemos error
        if (Boolean.FALSE.equals(updateUserPasswordRequest.getNewPassword().equals(updateUserPasswordRequest.getRepeatNewPassword()))) {
            throw new BadRequestException("New password and repeat password do not match");
        }

        // actualizamos
        this.userService.updatePassword(updateUserPasswordRequest.getUsername(), this.passwordEncoderService.encodePassword(updateUserPasswordRequest.getNewPassword()));
    }
}
