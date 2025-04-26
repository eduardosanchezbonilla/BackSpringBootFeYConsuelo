package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRehearsalProjectionListToEventResponseListConverter {

    private final MusicianRehearsalProjectionToEventResponseConverter musicianRehearsalProjectionToEventResponseConverter;

    public List<EventResponse> convert(final List<MusicianRehearsalProjection> musicianRehearsalEntityList) {
        if (CollectionUtils.isEmpty(musicianRehearsalEntityList)) {
            return List.of();
        }
        return musicianRehearsalEntityList.stream()
                .map(this.musicianRehearsalProjectionToEventResponseConverter::convert)
                .toList();
    }
}
