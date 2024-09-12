package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEntityListToMusicianResponseListConverter {

    private final MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;

    public List<MusicianResponse> convert(final List<MusicianEntity> musicianEntityList) {
        if (CollectionUtils.isEmpty(musicianEntityList)) {
            return List.of();
        }
        return musicianEntityList.stream()
                .map(this.musicianEntityToMusicianResponseConverter::convert)
                .toList();
    }
}
