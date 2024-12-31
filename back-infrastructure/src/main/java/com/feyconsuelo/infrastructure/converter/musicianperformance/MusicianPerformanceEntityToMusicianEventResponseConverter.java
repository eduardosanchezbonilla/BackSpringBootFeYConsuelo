package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityToMusicianResponseConverter;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianPerformanceEntityToMusicianEventResponseConverter {

    private final MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;
    private final MusicianPerformanceEntityToEventResponseConverter musicianPerformanceEntityToEventResponseConverter;

    public MusicianEventResponse convert(final MusicianPerformanceEntity performanceEntity) {
        final MusicianResponse musician = this.musicianEntityToMusicianResponseConverter.convert(performanceEntity.getMusician());
        final EventResponse event = this.musicianPerformanceEntityToEventResponseConverter.convert(performanceEntity);

        return MusicianEventResponse.builder()
                .musicianResponse(musician)
                .eventResponse(event)
                .build();
    }
}
