package com.feyconsuelo.apirest.service.musicianmarchsolo.query;

import com.feyconsuelo.apirest.converter.musiciansolostats.MusicianSoloStatsResponseListToMusicianSoloStatsResponseDtoListConverter;
import com.feyconsuelo.domain.model.musiciansolostats.MusicianSoloStatsResponse;
import com.feyconsuelo.domain.usecase.musiciansolostats.GetMusicianSoloStats;
import com.feyconsuelo.openapi.model.MusicianSolosStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetMusicianSoloStatsService {

    private final GetMusicianSoloStats getMusicianSoloStats;

    private final MusicianSoloStatsResponseListToMusicianSoloStatsResponseDtoListConverter musicianSoloStatsResponseListToMusicianSoloStatsResponseDtoListConverter;

    public ResponseEntity<List<MusicianSolosStatsResponseDto>> getMusicianSoloStats() {
        final List<MusicianSoloStatsResponse> musicianSoloStatsResponseList = this.getMusicianSoloStats.execute();
        if (CollectionUtils.isEmpty(musicianSoloStatsResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.musicianSoloStatsResponseListToMusicianSoloStatsResponseDtoListConverter.convert(musicianSoloStatsResponseList));
    }

}
