package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.GetMusician;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetMusicianImpl implements GetMusician {

    private final MusicianService musicianService;

    @Override
    public Optional<MusicianResponse> execute(final Long musicianId) {
        return this.musicianService.get(musicianId);
    }
}
