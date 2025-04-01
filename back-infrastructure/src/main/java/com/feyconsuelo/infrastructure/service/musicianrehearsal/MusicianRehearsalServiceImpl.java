package com.feyconsuelo.infrastructure.service.musicianrehearsal;

import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.converter.musicianrehearsal.MusicianEventRequestToMusicianRehearsalEntityConverter;
import com.feyconsuelo.infrastructure.converter.musicianrehearsal.MusicianRehearsalEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musicianrehearsal.MusicianRehearsalEntityListToMusicianEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musicianrehearsal.MusicianRehearsalProjectionListToMusicianEventResponseListConverter;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalPK;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalProjection;
import com.feyconsuelo.infrastructure.repository.MusicianRehearsalRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianRehearsalServiceImpl implements MusicianRehearsalService {

    private final MusicianRehearsalRepository musicianRehearsalRepository;
    private final MusicianEventRequestToMusicianRehearsalEntityConverter musicianEventRequestToMusicianRehearsalEntityConverter;
    private final MusicianRehearsalEntityListToEventResponseListConverter musicianRehearsalEntityListToEventResponseListConverter;
    private final MusicianRehearsalEntityListToMusicianEventResponseListConverter musicianRehearsalEntityListToMusicianEventResponseListConverter;
    private final MusicianRehearsalProjectionListToMusicianEventResponseListConverter musicianRehearsalProjectionListToMusicianEventResponseListConverter;

    @Override
    @Transactional
    public List<EventResponse> getAll(final Long musicianId, final LocalDate startDate, final LocalDate endDate) {
        final List<MusicianRehearsalEntity> rehearsalList = this.musicianRehearsalRepository.findAllActives(musicianId, startDate, endDate);
        return this.musicianRehearsalEntityListToEventResponseListConverter.convert(rehearsalList);
    }

    @Override
    public void save(final MusicianEventRequest musicianEventRequest) {
        final Optional<MusicianRehearsalEntity> event = this.musicianRehearsalRepository.findById(
                MusicianRehearsalPK.builder()
                        .musicianId(musicianEventRequest.getMusicianId())
                        .rehearsalId(musicianEventRequest.getEventId())
                        .build()
        );

        if (event.isEmpty()) {
            this.musicianRehearsalRepository.save(
                    this.musicianEventRequestToMusicianRehearsalEntityConverter.convert(musicianEventRequest)
            );
        } else {
            this.musicianRehearsalRepository.save(
                    this.musicianEventRequestToMusicianRehearsalEntityConverter.update(event.get())
            );
        }
    }

    @Override
    public void logicalDelete(final Long musicianId, final Long eventId) {
        final Optional<MusicianRehearsalEntity> event = this.musicianRehearsalRepository.findById(
                MusicianRehearsalPK.builder()
                        .musicianId(musicianId)
                        .rehearsalId(eventId)
                        .build()
        );

        event.ifPresent(musicianRehearsalEntity -> this.musicianRehearsalRepository.save(this.musicianEventRequestToMusicianRehearsalEntityConverter.deleteEntity(musicianRehearsalEntity)));
    }

    @Override
    public List<MusicianEventResponse> findAllActivesMusiciansLastRehearsalUntilDateTime(final LocalDateTime dateTime) {
        final List<MusicianRehearsalEntity> rehearsalList = this.musicianRehearsalRepository.findAllActivesMusiciansLastRehearsalUntilDateTime(dateTime);
        return this.musicianRehearsalEntityListToMusicianEventResponseListConverter.convert(rehearsalList);
    }

    @Override
    public List<MusicianEventResponse> findAllActivesMusiciansByRehearsalId(final Long rehearsalId, final Boolean returnFakeMusicians) {
        final List<MusicianRehearsalEntity> rehearsalList = this.musicianRehearsalRepository.findAllActivesMusiciansByRehearsalId(rehearsalId);
        final List<MusicianRehearsalProjection> rehearsalFakeMusicianList = Boolean.TRUE.equals(returnFakeMusicians) ?
                this.musicianRehearsalRepository.findAllActivesFakeMusiciansByRehearsalId(rehearsalId) :
                List.of();

        final List<MusicianEventResponse> musicianEventResponse = this.musicianRehearsalEntityListToMusicianEventResponseListConverter.convert(rehearsalList);
        final List<MusicianEventResponse> fakeMusicianEventResponse = this.musicianRehearsalProjectionListToMusicianEventResponseListConverter.convert(rehearsalFakeMusicianList);

        return Stream.concat(musicianEventResponse.stream(), fakeMusicianEventResponse.stream()).toList();
    }

}
