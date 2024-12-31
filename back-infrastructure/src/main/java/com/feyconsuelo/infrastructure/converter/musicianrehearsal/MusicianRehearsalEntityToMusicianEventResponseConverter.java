package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityToMusicianResponseConverter;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRehearsalEntityToMusicianEventResponseConverter {

    private final MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;
    private final MusicianRehearsalEntityToEventResponseConverter musicianRehearsalEntityToEventResponseConverter;

    public MusicianEventResponse convert(final MusicianRehearsalEntity rehearsalEntity) {
        final MusicianResponse musician = this.musicianEntityToMusicianResponseConverter.convert(rehearsalEntity.getMusician());
        final EventResponse event = this.musicianRehearsalEntityToEventResponseConverter.convert(rehearsalEntity);

        return MusicianEventResponse.builder()
                .musicianResponse(musician)
                .eventResponse(event)
                .build();
    }
}
