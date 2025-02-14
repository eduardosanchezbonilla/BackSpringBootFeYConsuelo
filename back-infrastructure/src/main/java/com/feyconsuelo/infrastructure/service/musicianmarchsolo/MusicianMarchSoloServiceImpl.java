package com.feyconsuelo.infrastructure.service.musicianmarchsolo;

import com.feyconsuelo.application.service.musicianmarchsolo.MusicianMarchSoloService;
import com.feyconsuelo.domain.model.musicianmarchsolo.MusicianMarchSoloResponse;
import com.feyconsuelo.infrastructure.converter.musicianmarchsolo.MusicianMarchSoloEntityListToMusicianMarchSoloResponseListConverter;
import com.feyconsuelo.infrastructure.entities.musicianmarchsolo.MusicianMarchSolo;
import com.feyconsuelo.infrastructure.repository.MusicianMarchSoloRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianMarchSoloServiceImpl implements MusicianMarchSoloService {

    private final MusicianMarchSoloRepository musicianMarchSoloRepository;
    private final MusicianMarchSoloEntityListToMusicianMarchSoloResponseListConverter musicianMarchSoloEntityListToMusicianMarchSoloResponseListConverter;

    @Override
    public List<MusicianMarchSoloResponse> getMusicianMarchSolo(final Long musicianId) {
        final List<MusicianMarchSolo> musicianMarchSoloList = this.musicianMarchSoloRepository.findMusicianMarchSolos(musicianId);
        return this.musicianMarchSoloEntityListToMusicianMarchSoloResponseListConverter.convert(musicianMarchSoloList);
    }

}
