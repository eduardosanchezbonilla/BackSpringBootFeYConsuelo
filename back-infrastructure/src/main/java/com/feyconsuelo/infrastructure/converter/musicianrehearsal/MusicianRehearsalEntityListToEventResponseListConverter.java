package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRehearsalEntityListToEventResponseListConverter {

    private final MusicianRehearsalEntityToEventResponseConverter musicianRehearsalEntityToEventResponseConverter;

    public List<EventResponse> convert(final List<MusicianRehearsalEntity> musicianRehearsalEntityList) {
        if (CollectionUtils.isEmpty(musicianRehearsalEntityList)) {
            return List.of();
        }
        return musicianRehearsalEntityList.stream()
                .map(this.musicianRehearsalEntityToEventResponseConverter::convert)
                .toList();
    }
}
