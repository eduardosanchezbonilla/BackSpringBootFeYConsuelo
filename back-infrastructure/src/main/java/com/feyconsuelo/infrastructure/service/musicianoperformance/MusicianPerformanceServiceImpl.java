package com.feyconsuelo.infrastructure.service.musicianoperformance;

import com.feyconsuelo.application.service.musicianperformance.MusicianPerformanceService;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.converter.musicianperformance.MusicianEventRequestToMusicianPerformanceEntityConverter;
import com.feyconsuelo.infrastructure.converter.musicianperformance.MusicianPerformanceEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musicianperformance.MusicianPerformanceProjectionListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musicianperformance.MusicianPerformanceProjectionListToMusicianEventResponseListConverter;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformancePK;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import com.feyconsuelo.infrastructure.repository.MusicianPerformanceRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianPerformanceServiceImpl implements MusicianPerformanceService {

    private final MusicianPerformanceRepository musicianPerformanceRepository;
    private final MusicianEventRequestToMusicianPerformanceEntityConverter musicianEventRequestToMusicianPerformanceEntityConverter;
    private final MusicianPerformanceEntityListToEventResponseListConverter musicianPerformanceEntityListToEventResponseListConverter;
    private final MusicianPerformanceProjectionListToMusicianEventResponseListConverter musicianPerformanceProjectionListToMusicianEventResponseListConverter;
    private final MusicianPerformanceProjectionListToEventResponseListConverter musicianPerformanceProjectionListToEventResponseListConverter;

    @Override
    public List<EventResponse> getAllMusicianPerformance(final Long musicianId, final LocalDate startDate, final LocalDate endDate) {
        final List<MusicianPerformanceProjection> rehearsalList = this.musicianPerformanceRepository.findAllMusicianPerformanceActives(
                musicianId,
                startDate,
                endDate,
                startDate == null,
                endDate == null
        );
        return this.musicianPerformanceProjectionListToEventResponseListConverter.convert(rehearsalList);
    }

    @Override
    public List<EventResponse> getAll(final Long musicianId, final LocalDate startDate, final LocalDate endDate) {
        final List<MusicianPerformanceEntity> rehearsalList = this.musicianPerformanceRepository.findAllActives(
                musicianId,
                startDate,
                endDate,
                startDate == null,
                endDate == null
        );
        return this.musicianPerformanceEntityListToEventResponseListConverter.convert(rehearsalList);
    }

    @Override
    public void save(final MusicianEventRequest musicianEventRequest) {
        // recupero el registro, porque si existe solo lo modifico
        final Optional<MusicianPerformanceEntity> event = this.musicianPerformanceRepository.findById(
                MusicianPerformancePK.builder()
                        .musicianId(musicianEventRequest.getMusicianId())
                        .performanceId(musicianEventRequest.getEventId())
                        .build()
        );

        if (event.isEmpty()) {
            this.musicianPerformanceRepository.save(
                    this.musicianEventRequestToMusicianPerformanceEntityConverter.convert(musicianEventRequest)
            );
        } else {
            this.musicianPerformanceRepository.save(
                    this.musicianEventRequestToMusicianPerformanceEntityConverter.update(event.get(), musicianEventRequest)
            );
        }
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

    @Override
    public List<MusicianEventResponse> findAllActivesMusiciansByPerformanceId(final Long performanceId, final Boolean returnFakeMusicians) {
        final List<MusicianPerformanceProjection> rehearsalProjectionList = this.musicianPerformanceRepository.findAllActivesMusiciansByPerformanceId1(performanceId);
        final List<MusicianPerformanceProjection> rehearsalFakeMusicianList = Boolean.TRUE.equals(returnFakeMusicians) ?
                this.musicianPerformanceRepository.findAllActivesFakeMusiciansByPerformanceId(performanceId) :
                List.of();

        final List<MusicianEventResponse> musicianEventResponse1 = this.musicianPerformanceProjectionListToMusicianEventResponseListConverter.convert(rehearsalProjectionList, Boolean.FALSE);
        final List<MusicianEventResponse> fakeMusicianEventResponse = this.musicianPerformanceProjectionListToMusicianEventResponseListConverter.convert(rehearsalFakeMusicianList, Boolean.TRUE);

        return Stream.concat(musicianEventResponse1.stream(), fakeMusicianEventResponse.stream()).toList();
    }

}
