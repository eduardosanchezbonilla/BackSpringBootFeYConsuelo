package com.feyconsuelo.domain.usecase.musicianinventory;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;

import java.util.List;

public interface GetMusicianInventories {

    List<MusicianInventoryResponse> execute(Long musicianId);

}
