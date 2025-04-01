package com.feyconsuelo.infrastructure.converter.musicianrehearsal;

import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRehearsalProjectionListToMusicianEventResponseListConverter {

    private final MusicianRehearsalProjectionToMusicianEventResponseConverter musicianRehearsalProjectionToMusicianEventResponseConverter;

    public List<MusicianEventResponse> convert(final List<MusicianRehearsalProjection> musicianRehearsalProjectionList) {
        if (CollectionUtils.isEmpty(musicianRehearsalProjectionList)) {
            return List.of();
        }
        return musicianRehearsalProjectionList.stream()
                .map(this.musicianRehearsalProjectionToMusicianEventResponseConverter::convert)
                .toList();
    }
}
