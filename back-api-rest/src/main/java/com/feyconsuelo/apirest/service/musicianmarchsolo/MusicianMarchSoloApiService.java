package com.feyconsuelo.apirest.service.musicianmarchsolo;

import com.feyconsuelo.apirest.service.musicianmarchsolo.query.GetMusicianMarchSoloService;
import com.feyconsuelo.openapi.api.MusicianSoloControllerApiDelegate;
import com.feyconsuelo.openapi.model.MusicianMarchSoloResponseDto;
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

    @Override
    public ResponseEntity<List<MusicianMarchSoloResponseDto>> getMusicianSolos(final Long musicianId) {
        return this.getMusicianMarchSoloService.getMusicianMarchSolo(musicianId);
    }

}
