package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceEntityListToMusicianEventResponseListConverter {

    private final MusicianPerformanceEntityToMusicianEventResponseConverter musicianPerformanceEntityToMusicianEventResponseConverter;

    public List<MusicianEventResponse> convert(final List<MusicianPerformanceEntity> musicianPerformanceEntityList) {
        if (CollectionUtils.isEmpty(musicianPerformanceEntityList)) {
            return List.of();
        }
        return musicianPerformanceEntityList.stream()
                .map(this.musicianPerformanceEntityToMusicianEventResponseConverter::convert)
                .toList();
    }
}
