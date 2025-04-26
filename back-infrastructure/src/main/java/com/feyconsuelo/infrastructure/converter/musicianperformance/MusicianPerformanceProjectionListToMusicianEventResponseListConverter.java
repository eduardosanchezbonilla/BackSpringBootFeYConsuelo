package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceProjectionListToMusicianEventResponseListConverter {

    private final MusicianPerformanceProjectionToMusicianEventResponseConverter musicianPerformanceProjectionToMusicianEventResponseConverter;

    public List<MusicianEventResponse> convert(final List<MusicianPerformanceProjection> musicianPerformanceProjectionList, final Boolean musicianFake) {
        if (CollectionUtils.isEmpty(musicianPerformanceProjectionList)) {
            return List.of();
        }
        return musicianPerformanceProjectionList.stream()
                .map(m -> this.musicianPerformanceProjectionToMusicianEventResponseConverter.convert(m, musicianFake))
                .toList();
    }
}
