package com.feyconsuelo.application.usecase.musicianinventory;

import com.feyconsuelo.application.service.musicianinventory.MusicianInventoryService;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;
import com.feyconsuelo.domain.usecase.musicianinventory.InsertMusicianInventory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertMusicianInventoryImpl implements InsertMusicianInventory {

    private final MusicianInventoryService musicianInventoryService;

    @Override
    public void execute(final MusicianInventoryRequest musicianInventoryRequest) {

        this.musicianInventoryService.insert(musicianInventoryRequest);
    }
}
