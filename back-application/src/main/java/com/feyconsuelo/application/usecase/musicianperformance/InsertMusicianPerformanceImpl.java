package com.feyconsuelo.application.usecase.musicianperformance;

import com.feyconsuelo.application.service.musicianperformance.MusicianPerformanceService;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertMusicianPerformanceImpl {

    private final MusicianPerformanceService musicianPerformanceService;

    public void insertMusicianPerformance(final MusicianEventRequest musicianEventRequest) {

        // insertamos
        this.musicianPerformanceService.save(musicianEventRequest);

    }

}
