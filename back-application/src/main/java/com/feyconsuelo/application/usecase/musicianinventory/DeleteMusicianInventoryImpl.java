package com.feyconsuelo.application.usecase.musicianinventory;

import com.feyconsuelo.application.service.musicianinventory.MusicianInventoryService;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;
import com.feyconsuelo.domain.usecase.musicianinventory.DeleteMusicianInventory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteMusicianInventoryImpl implements DeleteMusicianInventory {

    private final MusicianInventoryService musicianInventoryService;

    @Override
    public void execute(final MusicianInventoryRequest musicianInventoryRequest) {
        this.musicianInventoryService.logicalDelete(musicianInventoryRequest);
    }
}
