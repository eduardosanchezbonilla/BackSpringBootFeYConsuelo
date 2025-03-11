package com.feyconsuelo.infrastructure.service.musiciansolostats;

import com.feyconsuelo.application.service.musiciansolostats.MusicianSoloStatsService;
import com.feyconsuelo.domain.model.musiciansolostats.MusicianSoloStatsResponse;
import com.feyconsuelo.infrastructure.converter.musiciansolostats.MusicianSoloStatsEntityListToMusicianSoloStatsResponseListConverter;
import com.feyconsuelo.infrastructure.entities.musiciansolostats.MusicianSoloStats;
import com.feyconsuelo.infrastructure.repository.MusicianSoloStatsRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianSoloStatsServiceImpl implements MusicianSoloStatsService {

    private final MusicianSoloStatsRepository musicianSoloStatsRepository;
    private final MusicianSoloStatsEntityListToMusicianSoloStatsResponseListConverter musicianSoloStatsEntityListToMusicianSoloStatsResponseListConverter;

    @Override
    public List<MusicianSoloStatsResponse> getMusicianSoloStats() {
        final List<MusicianSoloStats> musicianSoloStatsList = this.musicianSoloStatsRepository.getMusicianSoloStats();
        return this.musicianSoloStatsEntityListToMusicianSoloStatsResponseListConverter.convert(musicianSoloStatsList);
    }

}
