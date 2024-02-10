package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.exception.FeYConsueloNotFoundException;
import com.feyconsuelo.domain.usecase.DeleteMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteMusicianImpl implements DeleteMusician {

    private final MusicianRepository musicianRepository;

    @Override
    public void execute(final String musicianId) {

        final Musician musician = this.musicianRepository.get(musicianId)
                .orElseThrow(() -> new FeYConsueloNotFoundException("Musician with id: " + musicianId + " not found"));

        this.musicianRepository.delete(musician.getId());

    }
}
