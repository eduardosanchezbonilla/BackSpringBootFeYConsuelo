package com.feyconsuelo.apirest.service.musicianmarchsolo.query;

import com.feyconsuelo.apirest.converter.musicianmarchsolo.MusicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter;
import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;
import com.feyconsuelo.domain.usecase.musicianmarchsolo.GetMusicianMarchSolo;
import com.feyconsuelo.openapi.model.MusicianMarchSoloResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetMusicianMarchSoloService {

    private final GetMusicianMarchSolo getMusicianMarchSolo;

    private final MusicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter musicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter;

    public ResponseEntity<List<MusicianMarchSoloResponseDto>> getMusicianMarchSolo(final Long musicianId) {
        final List<MusicianMarchSoloResponse> musicianMarchSoloResponseList = this.getMusicianMarchSolo.execute(musicianId);
        if (CollectionUtils.isEmpty(musicianMarchSoloResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.musicianMarchSoloResponseListToMusicianMarchSoloResponseDtoListConverter.convert(musicianMarchSoloResponseList));
    }

}
