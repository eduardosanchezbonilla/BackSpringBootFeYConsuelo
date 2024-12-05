package com.feyconsuelo.infrastructure.converter.musicianperformance;

import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformancePK;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEventRequestToMusicianPerformanceEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public MusicianPerformanceEntity convert(final MusicianEventRequest musicianEventRequest) {
        return MusicianPerformanceEntity.builder()
                .id(
                        MusicianPerformancePK.builder()
                                .musicianId(musicianEventRequest.getMusicianId())
                                .performanceId(musicianEventRequest.getEventId())
                                .build()
                )
                .musician(
                        MusicianEntity.builder()
                                .id(musicianEventRequest.getMusicianId())
                                .build()
                )
                .performance(
                        PerformanceEntity.builder()
                                .id(musicianEventRequest.getEventId())
                                .build()
                )
                .bus(musicianEventRequest.getBus())
                .updateUserMP(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public MusicianPerformanceEntity deleteEntity(final MusicianPerformanceEntity musicianPerformanceEntity) {
        musicianPerformanceEntity.setDeleteDateMP(LocalDateTime.now());
        musicianPerformanceEntity.setUpdateUserMP(this.tokenInfoExtractorService.getUsername());
        return musicianPerformanceEntity;
    }
}
