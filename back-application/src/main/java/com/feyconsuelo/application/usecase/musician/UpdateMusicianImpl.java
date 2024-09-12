package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.voice.GetVoiceImpl;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.UpdateMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateMusicianImpl implements UpdateMusician {

    private final MusicianService musicianService;

    private final InsertMusicianImpl insertMusician;

    private final DeleteMusicianImpl deleteMusician;

    private final GetVoiceImpl getVoice;

    @Override
    public MusicianResponse execute(final Long musicianId, final MusicianRequest musicianRequest) {

        final Optional<MusicianResponse> musicianOptional = this.musicianService.get(musicianId);

        if (musicianOptional.isEmpty()) {
            throw new NotFoundException("Musician register to update not found");
        }

        // si ya existe un musico con ese dni, lanzamos error
        final Optional<MusicianResponse> findMusician = this.musicianService.getByDni(musicianRequest.getDni());

        if (findMusician.isPresent() && Boolean.FALSE.equals(musicianId.equals(findMusician.get().getId()))) {
            throw new BadRequestException("There is already another musician with that DNI");
        }

        // comprobamos si la voz que estan pasando existe, sino devolvemos error
        final var voice = this.getVoice.execute(musicianRequest.getVoiceId());

        if (voice.isEmpty()) {
            throw new BadRequestException("The entered voice does not exist");
        }

        final String oldDni = musicianOptional.get().getDni();
        final MusicianResponse updatedMusicianRequest = this.musicianService.update(musicianId, musicianRequest);

        // si ha cambiado el DNI, entonces tenemos que borrar el usuario asociado al antiguo DNI, y crear el usuario asociado al nuevo DNI
        this.updateUserAssociatedToMusician(oldDni, updatedMusicianRequest.getDni());

        return updatedMusicianRequest;

    }

    private void updateUserAssociatedToMusician(final String oldDni, final String newDni) {
        final String oldUsername = oldDni.toLowerCase();
        final String newUsername = newDni.toLowerCase();

        if (oldUsername.equals(newUsername)) {
            return;
        }

        // si ha cambiado
        // borramos el usuario asociado al antiguo DNI
        this.deleteMusician.deleteUserAssociatedToMusician(oldUsername);

        // insertamos el nuevo usuario
        this.insertMusician.createUserAssociatedToMusician(newUsername);
    }

}
