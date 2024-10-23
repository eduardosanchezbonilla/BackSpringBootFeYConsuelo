package com.feyconsuelo.domain.usecase.musicianinventory;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;

public interface DeleteMusicianInventory {

    void execute(MusicianInventoryRequest musicianInventoryRequest);

}
