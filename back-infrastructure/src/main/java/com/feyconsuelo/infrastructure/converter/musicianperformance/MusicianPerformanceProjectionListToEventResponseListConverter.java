package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceProjectionListToEventResponseListConverter {

    private final MusicianPerformanceProjectionToEventResponseConverter musicianPerformanceProjectionToEventResponseConverter;

    public List<EventResponse> convert(final List<MusicianPerformanceProjection> musicianRehearsalEntityList) {
        if (CollectionUtils.isEmpty(musicianRehearsalEntityList)) {
            return List.of();
        }
        return musicianRehearsalEntityList.stream()
                .map(this.musicianPerformanceProjectionToEventResponseConverter::convert)
                .toList();
    }
}
