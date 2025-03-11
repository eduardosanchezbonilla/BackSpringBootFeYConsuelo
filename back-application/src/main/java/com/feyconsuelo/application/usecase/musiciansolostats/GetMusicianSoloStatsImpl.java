package com.feyconsuelo.application.usecase.musiciansolostats;

import com.feyconsuelo.application.service.musiciansolostats.MusicianSoloStatsService;
import com.feyconsuelo.domain.model.musiciansolostats.MusicianSoloStatsResponse;
import com.feyconsuelo.domain.usecase.musiciansolostats.GetMusicianSoloStats;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetMusicianSoloStatsImpl implements GetMusicianSoloStats {

    private final MusicianSoloStatsService musicianSoloStatsService;

    @Override
    public List<MusicianSoloStatsResponse> execute() {
        return this.musicianSoloStatsService.getMusicianSoloStats();
    }
}
