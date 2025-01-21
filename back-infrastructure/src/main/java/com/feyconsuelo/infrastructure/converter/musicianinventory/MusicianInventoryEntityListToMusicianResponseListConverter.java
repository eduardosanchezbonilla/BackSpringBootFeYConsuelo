package com.feyconsuelo.infrastructure.converter.musicianinventory;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityToMusicianResponseConverter;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianInventoryEntityListToMusicianResponseListConverter {

    private final MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;

    public List<MusicianResponse> convert(final List<MusicianInventoryEntity> musicianInventoryEntityList) {
        if (CollectionUtils.isEmpty(musicianInventoryEntityList)) {
            return List.of();
        }
        return musicianInventoryEntityList.stream()
                .map(MusicianInventoryEntity::getMusician)
                .map(musician -> this.musicianEntityToMusicianResponseConverter.convert(musician, Boolean.TRUE))
                .distinct()
                .toList();
    }
}
