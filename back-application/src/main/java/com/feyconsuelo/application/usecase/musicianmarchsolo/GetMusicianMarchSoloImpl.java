package com.feyconsuelo.application.usecase.musicianmarchsolo;

import com.feyconsuelo.application.service.musicianmarchsolo.MusicianMarchSoloService;
import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;
import com.feyconsuelo.domain.usecase.musicianmarchsolo.GetMusicianMarchSolo;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetMusicianMarchSoloImpl implements GetMusicianMarchSolo {

    private final MusicianMarchSoloService musicianMarchSoloService;

    @Override
    public List<MusicianMarchSoloResponse> execute(final Long musicianId) {
        return this.musicianMarchSoloService.getMusicianMarchSolo(musicianId);
    }
}
