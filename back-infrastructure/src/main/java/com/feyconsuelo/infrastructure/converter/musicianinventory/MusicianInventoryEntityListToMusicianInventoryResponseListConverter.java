package com.feyconsuelo.infrastructure.converter.musicianinventory;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianInventoryEntityListToMusicianInventoryResponseListConverter {

    private final MusicianInventoryEntityToMusicianInventoryResponseConverter musicianInventoryEntityToMusicianInventoryResponseConverter;

    public List<MusicianInventoryResponse> convert(final List<MusicianInventoryEntity> musicianInventoryEntityList) {
        if (CollectionUtils.isEmpty(musicianInventoryEntityList)) {
            return List.of();
        }
        return musicianInventoryEntityList.stream()
                .map(this.musicianInventoryEntityToMusicianInventoryResponseConverter::convert)
                .toList();
    }
}
