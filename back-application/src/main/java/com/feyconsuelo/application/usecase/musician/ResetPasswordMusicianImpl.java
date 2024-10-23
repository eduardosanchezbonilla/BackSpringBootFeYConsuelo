package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.user.GetUserImpl;
import com.feyconsuelo.application.usecase.user.UpdateUserPasswordImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.musician.ResetPasswordMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class ResetPasswordMusicianImpl implements ResetPasswordMusician {

    private final MusicianService musicianService;

    private final GetUserImpl getUser;

    private final UpdateUserPasswordImpl updateUserPassword;

    private final InsertMusicianImpl insertMusician;

    private final DeleteMusicianImpl deleteMusician;

    @Override
    public void execute(final String dni) {
        // comprobamos si el musico existe
        final Optional<MusicianResponse> findMusician = this.musicianService.getByDni(dni.toUpperCase());

        if (findMusician.isEmpty()) {
            throw new NotFoundException("No existe ningun musico con ese DNI");
        }

        // comprobamos si existe el musico
        final Optional<UserResponse> user = this.getUser.execute(dni.toLowerCase());

        // sino existe el usuario devolvemos error de NotFound
        if (user.isEmpty()) {
            throw new NotFoundException("No existe el usuario que intenta modificar");
        }

        // borramos el usuario asociado al antiguo DNI
        this.deleteMusician.deleteUserAssociatedToMusician(dni.toLowerCase());

        // insertamos el nuevo usuario
        this.insertMusician.createUserAssociatedToMusician(dni.toLowerCase());

    }

}
