package com.feyconsuelo.infrastructure.converter.musicianinventory;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryEntity;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryPK;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianInventoryRequestToMusicianInventoryEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public MusicianInventoryEntity convert(final MusicianInventoryRequest musicianInventoryRequest) {
        return MusicianInventoryEntity.builder()
                .id(
                        MusicianInventoryPK.builder()
                                .musicianId(musicianInventoryRequest.getMusicianId())
                                .inventoryId(musicianInventoryRequest.getInventoryId())
                                .build()
                )
                .deleteDate(musicianInventoryRequest.getDeletedDate())
                .updateUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

}
