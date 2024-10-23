package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.user.DeleteUserImpl;
import com.feyconsuelo.application.usecase.user.GetUserImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.musician.DeleteMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class DeleteMusicianImpl implements DeleteMusician {

    private final MusicianService musicianService;

    private final DeleteUserImpl deleteUser;

    private final GetUserImpl getUser;

    @Override
    public void execute(final Long musicianId) {
        final Optional<MusicianResponse> musicianOptional = this.musicianService.get(musicianId);

        if (musicianOptional.isEmpty()) {
            throw new NotFoundException("No existe ningún músico con el iID introducido");
        } else {
            this.musicianService.logicalDelete(musicianId);
            this.deleteUserAssociatedToMusician(musicianOptional.get().getDni());
        }
    }

    public void deleteUserAssociatedToMusician(final String dni) {
        final String username = dni.toLowerCase();

        final Optional<UserResponse> user = this.getUser.execute(username);

        if (user.isPresent()) {
            // sino existe lo creo
            this.deleteUser.execute(username);
        }
    }
}
