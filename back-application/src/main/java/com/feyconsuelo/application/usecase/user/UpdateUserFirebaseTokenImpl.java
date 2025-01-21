package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.user.UpdateUserFirebaseTokenRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.UpdateUserFirebaseToken;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateUserFirebaseTokenImpl implements UpdateUserFirebaseToken {

    private final UserService userService;

    @Override
    public void execute(final UpdateUserFirebaseTokenRequest updateUserFirebaseTokenRequest) {

        final Optional<UserResponse> userOptional = this.userService.get(updateUserFirebaseTokenRequest.getUsername(), Boolean.TRUE);

        // sino existe el usuario devolvemos error de NotFound
        if (userOptional.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta modificar");
        }

        final List<String> firebaseTokenList = new ArrayList<>();
        if (userOptional.get().getFirebaseToken() != null) {
            firebaseTokenList.addAll(userOptional.get().getFirebaseToken());
        }

        // si el usuario ya tiene ese token, no hacemos nada
        if (Boolean.TRUE.equals(firebaseTokenList.contains(updateUserFirebaseTokenRequest.getFirebaseToken()))) {
            return;
        }

        // actualizamos
        firebaseTokenList.add(updateUserFirebaseTokenRequest.getFirebaseToken());
        this.userService.updateFirebaseToken(
                updateUserFirebaseTokenRequest.getUsername(),
                firebaseTokenList
        );
    }
}