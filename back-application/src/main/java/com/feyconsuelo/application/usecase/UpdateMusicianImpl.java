package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.exception.FeYConsueloNotFoundException;
import com.feyconsuelo.domain.usecase.UpdateMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateMusicianImpl implements UpdateMusician {

    private final MusicianRepository musicianRepository;

    @Override
    public Musician execute(final String musicianId, final Musician musician) {

        final Optional<Musician> musicianOptional = this.musicianRepository.get(musicianId);

        if (musicianOptional.isEmpty()) {
            throw new FeYConsueloNotFoundException("Musician register to update not found");
        } else {
            musicianOptional.get().setDni(musician.getDni());
            musicianOptional.get().setName(musician.getName());
            musicianOptional.get().setSurname(musician.getSurname());
            musicianOptional.get().setDirection(musician.getDirection());
            musicianOptional.get().setMunicipality(musician.getMunicipality());
            musicianOptional.get().setProvince(musician.getProvince());
            musicianOptional.get().setVoice(musician.getVoice());
            musicianOptional.get().setImage(musician.getImage());
            return this.musicianRepository.save(musicianOptional.get());
        }
    }
}
