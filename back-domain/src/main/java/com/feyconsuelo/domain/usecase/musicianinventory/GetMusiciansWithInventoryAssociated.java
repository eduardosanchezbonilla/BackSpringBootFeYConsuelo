package com.feyconsuelo.domain.usecase.musicianinventory;

import com.feyconsuelo.domain.model.musician.MusicianResponse;

import java.util.List;

public interface GetMusiciansWithInventoryAssociated {

    List<MusicianResponse> execute(Long inventoryId);

}
