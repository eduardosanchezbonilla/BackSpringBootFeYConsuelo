package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.GetMusicianFromDni;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetMusicianFromDniImpl implements GetMusicianFromDni {

    private final MusicianService musicianService;

    @Override
    public Optional<MusicianResponse> execute(final String musicianDni) {
        return this.musicianService.getByDni(musicianDni, Boolean.TRUE);
    }
}
