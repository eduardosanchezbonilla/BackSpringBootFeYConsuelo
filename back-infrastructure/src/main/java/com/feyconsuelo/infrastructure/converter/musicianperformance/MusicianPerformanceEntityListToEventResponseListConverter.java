package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceEntityListToEventResponseListConverter {

    private final MusicianPerformanceEntityToEventResponseConverter musicianPerformanceEntityToEventResponseConverter;

    public List<EventResponse> convert(final List<MusicianPerformanceEntity> musicianRehearsalEntityList) {
        if (CollectionUtils.isEmpty(musicianRehearsalEntityList)) {
            return List.of();
        }
        return musicianRehearsalEntityList.stream()
                .map(this.musicianPerformanceEntityToEventResponseConverter::convert)
                .toList();
    }
}
