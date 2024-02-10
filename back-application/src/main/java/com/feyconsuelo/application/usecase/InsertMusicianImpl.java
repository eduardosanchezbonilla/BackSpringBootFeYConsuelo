package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.InsertMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertMusicianImpl implements InsertMusician {

    private final MusicianRepository musicianRepository;

    @Override
    public Musician execute(final Musician musician) {
        return this.musicianRepository.save(musician);
    }
}
