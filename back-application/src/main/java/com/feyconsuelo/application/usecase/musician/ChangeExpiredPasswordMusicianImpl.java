package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.user.GetUserImpl;
import com.feyconsuelo.application.usecase.user.UpdateUserPasswordImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianChangeExpiredPasswordRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UpdateUserPasswordRequest;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.musician.ChangeExpiredPasswordMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class ChangeExpiredPasswordMusicianImpl implements ChangeExpiredPasswordMusician {

    private final MusicianService musicianService;

    private final GetUserImpl getUser;

    private final UpdateUserPasswordImpl updateUserPassword;

    @Override
    public void execute(final String dni,
                        final MusicianChangeExpiredPasswordRequest musicianChangeExpiredPasswordRequest
    ) {
        // comprobamos si el musico existe
        final Optional<MusicianResponse> findMusician = this.musicianService.getByDni(dni.toUpperCase());

        if (findMusician.isEmpty()) {
            throw new NotFoundException("No existe ningun musico con ese DNI");
        }

        // comprobamos si realmente el password estaba expirado o no
        final Optional<UserResponse> user = this.getUser.execute(dni.toLowerCase());

        // sino existe el usuario devolvemos error de NotFound
        if (user.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta modificar");
        }
        if (Boolean.FALSE.equals(user.get().getPasswordExpired())) {
            throw new BadRequestException("El password del usuario no estaba expirado");
        }

        // actualizamos el password
        this.updateUserPassword.execute(
                UpdateUserPasswordRequest.builder()
                        .username(dni.toLowerCase())
                        .currentPassword(musicianChangeExpiredPasswordRequest.getCurrentPassword())
                        .newPassword(musicianChangeExpiredPasswordRequest.getNewPassword())
                        .repeatNewPassword(musicianChangeExpiredPasswordRequest.getRepeatNewPassword())
                        .build()
        );

    }

}
