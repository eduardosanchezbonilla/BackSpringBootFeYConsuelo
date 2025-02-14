package com.feyconsuelo.application.service.musicianmarchsolo;

import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;

import java.util.List;

public interface MusicianMarchSoloService {

    List<MusicianMarchSoloResponse> getMusicianMarchSolo(final Long musicianId);

}
