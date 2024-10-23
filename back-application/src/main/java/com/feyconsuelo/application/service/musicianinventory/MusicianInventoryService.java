package com.feyconsuelo.application.service.musicianinventory;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;

import java.util.List;

public interface MusicianInventoryService {

    void insert(MusicianInventoryRequest musicianInventoryRequest);

    void delete(MusicianInventoryRequest musicianInventoryRequest);

    void logicalDelete(MusicianInventoryRequest musicianInventoryRequest);

    List<MusicianInventoryResponse> getAllMusicianInventories(Long musicianId);

    List<MusicianResponse> getMusiciansWithInventory(final Long inventoryId);
}
