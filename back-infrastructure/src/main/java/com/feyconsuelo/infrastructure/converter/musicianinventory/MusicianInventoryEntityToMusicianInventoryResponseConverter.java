package com.feyconsuelo.infrastructure.converter.musicianinventory;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianInventoryEntityToMusicianInventoryResponseConverter {

    public MusicianInventoryResponse convert(final MusicianInventoryEntity musicianInventoryEntity) {
        return MusicianInventoryResponse.builder()
                .musicianId(musicianInventoryEntity.getId().getMusicianId())
                .inventoryId(musicianInventoryEntity.getId().getInventoryId())
                .inventoryName(musicianInventoryEntity.getInventory().getName())
                .inventoryImage(musicianInventoryEntity.getInventory().getImage())
                .deleteDate(musicianInventoryEntity.getDeleteDate())
                .build();
    }

}
