package com.feyconsuelo.application.usecase.musicianperformance;

import com.feyconsuelo.application.service.musicianperformance.MusicianPerformanceService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteMusicianPerformanceImpl {

    private final MusicianPerformanceService musicianPerformanceService;

    public void execute(final Long musicianId, final Long eventId) {
        this.musicianPerformanceService.logicalDelete(musicianId, eventId);
    }

}
