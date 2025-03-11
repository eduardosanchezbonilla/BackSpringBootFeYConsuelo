package com.feyconsuelo.infrastructure.converter.musiciansolostats;

import com.feyconsuelo.domain.model.musiciansolostats.MusicianSoloStatsResponse;
import com.feyconsuelo.infrastructure.entities.musiciansolostats.MusicianSoloStats;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianSoloStatsEntityListToMusicianSoloStatsResponseListConverter {

    public List<MusicianSoloStatsResponse> convert(final List<MusicianSoloStats> musicianSoloStatsList) {
        if (CollectionUtils.isEmpty(musicianSoloStatsList)) {
            return List.of();
        }

        return musicianSoloStatsList.stream()
                .map(
                        musicianSoloStats -> MusicianSoloStatsResponse.builder()
                                .id(musicianSoloStats.getId())
                                .name(musicianSoloStats.getName())
                                .surname(musicianSoloStats.getSurname())
                                .totalSolos(musicianSoloStats.getTotalSolos())
                                .totalMainSolos(musicianSoloStats.getTotalMainSolos())
                                .totalSecondarySolos(musicianSoloStats.getTotalSecondarySolos())
                                .build()
                )
                .toList();
    }
}
