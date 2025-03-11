package com.feyconsuelo.apirest.service.musicianmarchsolo;

import com.feyconsuelo.apirest.service.musicianmarchsolo.query.GetMusicianMarchSoloService;
import com.feyconsuelo.apirest.service.musicianmarchsolo.query.GetMusicianSoloStatsService;
import com.feyconsuelo.openapi.api.MusicianSoloControllerApiDelegate;
import com.feyconsuelo.openapi.model.MusicianMarchSoloResponseDto;
import com.feyconsuelo.openapi.model.MusicianSolosStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianMarchSoloApiService implements MusicianSoloControllerApiDelegate {

    private final GetMusicianMarchSoloService getMusicianMarchSoloService;
    private final GetMusicianSoloStatsService getMusicianSoloStatsService;

    @Override
    public ResponseEntity<List<MusicianMarchSoloResponseDto>> getMusicianSolos(final Long musicianId) {
        return this.getMusicianMarchSoloService.getMusicianMarchSolo(musicianId);
    }

    @Override
    public ResponseEntity<List<MusicianSolosStatsResponseDto>> getMusicianSolosStats() {
        return this.getMusicianSoloStatsService.getMusicianSoloStats();
    }

}
