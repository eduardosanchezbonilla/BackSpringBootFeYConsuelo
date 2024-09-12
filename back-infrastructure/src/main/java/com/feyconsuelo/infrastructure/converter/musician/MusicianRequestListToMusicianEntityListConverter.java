package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRequestListToMusicianEntityListConverter {

    private final MusicianRequestToMusicianEntityConverter musicianRequestToMusicianEntityConverter;

    public List<MusicianEntity> convert(final List<MusicianRequest> musicianRequestList) {
        if (CollectionUtils.isEmpty(musicianRequestList)) {
            return List.of();
        }
        return musicianRequestList.stream()
                .map(this.musicianRequestToMusicianEntityConverter::convert)
                .toList();
    }
}
