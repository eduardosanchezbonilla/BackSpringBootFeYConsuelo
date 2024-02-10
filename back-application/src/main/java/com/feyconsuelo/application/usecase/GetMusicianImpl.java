package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.GetMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetMusicianImpl implements GetMusician {

    private final MusicianRepository musicianRepository;

    @Override
    public Optional<Musician> execute(final String musicianId) {
        return this.musicianRepository.get(musicianId);
    }
}
