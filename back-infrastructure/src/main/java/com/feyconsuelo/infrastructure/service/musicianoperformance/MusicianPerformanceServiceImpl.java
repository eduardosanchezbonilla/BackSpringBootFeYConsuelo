package com.feyconsuelo.infrastructure.service.musicianoperformance;

import com.feyconsuelo.application.service.musicianperformance.MusicianPerformanceService;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.infrastructure.converter.musicianperformance.MusicianEventRequestToMusicianPerformanceEntityConverter;
import com.feyconsuelo.infrastructure.converter.musicianperformance.MusicianPerformanceEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformancePK;
import com.feyconsuelo.infrastructure.repository.MusicianPerformanceRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianPerformanceServiceImpl implements MusicianPerformanceService {

    private final MusicianPerformanceRepository musicianPerformanceRepository;
    private final MusicianEventRequestToMusicianPerformanceEntityConverter musicianEventRequestToMusicianPerformanceEntityConverter;
    private final MusicianPerformanceEntityListToEventResponseListConverter musicianPerformanceEntityListToEventResponseListConverter;

    @Override
    public List<EventResponse> getAll(final Long musicianId, final LocalDate startDate, final LocalDate endDate) {
        final List<MusicianPerformanceEntity> rehearsalList = this.musicianPerformanceRepository.findAllActives(musicianId, startDate, endDate);
        return this.musicianPerformanceEntityListToEventResponseListConverter.convert(rehearsalList);
    }

    @Override
    public void save(final MusicianEventRequest musicianEventRequest) {
        this.musicianPerformanceRepository.save(
                this.musicianEventRequestToMusicianPerformanceEntityConverter.convert(musicianEventRequest)
        );
    }

    @Override
    public void logicalDelete(final Long musicianId, final Long eventId) {
        final Optional<MusicianPerformanceEntity> event = this.musicianPerformanceRepository.findById(
                MusicianPerformancePK.builder()
                        .musicianId(musicianId)
                        .performanceId(eventId)
                        .build()
        );

        event.ifPresent(musicianPerformanceEntity -> this.musicianPerformanceRepository.save(this.musicianEventRequestToMusicianPerformanceEntityConverter.deleteEntity(musicianPerformanceEntity)));


    }

}
