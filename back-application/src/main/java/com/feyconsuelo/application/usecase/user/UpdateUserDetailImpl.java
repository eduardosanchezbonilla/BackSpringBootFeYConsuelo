package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.security.PasswordEncoderService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.user.UpdateUserDetailRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.UpdateUserDetail;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateUserDetailImpl implements UpdateUserDetail {

    private final UserService userService;

    private final PasswordEncoderService passwordEncoderService;

    private final ResizeImageImpl resizeImageService;

    @Override
    public void execute(final UpdateUserDetailRequest updateUserDetailRequest) {

        final Optional<UserResponse> userOptional = this.userService.get(updateUserDetailRequest.getUsername());

        // sino existe el usuario devolvemos error de NotFound
        if (userOptional.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta modificar");
        }

        // actualizamos
        this.userService.updateDetail(updateUserDetailRequest.getUsername(), updateUserDetailRequest);

    }
}
