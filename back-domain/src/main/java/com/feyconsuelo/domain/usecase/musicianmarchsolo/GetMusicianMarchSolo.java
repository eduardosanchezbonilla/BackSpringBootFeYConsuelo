package com.feyconsuelo.domain.usecase.musicianmarchsolo;

import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;

import java.util.List;

public interface GetMusicianMarchSolo {

    List<MusicianMarchSoloResponse> execute(Long musicianId);

}
