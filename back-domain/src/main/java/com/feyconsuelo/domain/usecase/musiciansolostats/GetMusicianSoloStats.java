package com.feyconsuelo.domain.usecase.musiciansolostats;

import com.feyconsuelo.domain.model.musiciansolostats.MusicianSoloStatsResponse;

import java.util.List;

public interface GetMusicianSoloStats {

    List<MusicianSoloStatsResponse> execute();

}
