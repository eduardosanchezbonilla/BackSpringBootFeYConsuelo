package com.feyconsuelo.infrastructure.service.musicianrehearsal;

import com.feyconsuelo.application.service.musicianrehearsal.MusicianRehearsalService;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;
import com.feyconsuelo.infrastructure.converter.musicianrehearsal.MusicianEventRequestToMusicianRehearsalEntityConverter;
import com.feyconsuelo.infrastructure.converter.musicianrehearsal.MusicianRehearsalEntityListToEventResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musicianrehearsal.MusicianRehearsalEntityListToMusicianEventResponseListConverter;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalPK;
import com.feyconsuelo.infrastructure.repository.MusicianRehearsalRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianRehearsalServiceImpl implements MusicianRehearsalService {

    private final MusicianRehearsalRepository musicianRehearsalRepository;
    private final MusicianEventRequestToMusicianRehearsalEntityConverter musicianEventRequestToMusicianRehearsalEntityConverter;
    private final MusicianRehearsalEntityListToEventResponseListConverter musicianRehearsalEntityListToEventResponseListConverter;
    private final MusicianRehearsalEntityListToMusicianEventResponseListConverter musicianRehearsalEntityListToMusicianEventResponseListConverter;

    @Override
    public List<EventResponse> getAll(final Long musicianId, final LocalDate startDate, final LocalDate endDate) {
        final List<MusicianRehearsalEntity> rehearsalList = this.musicianRehearsalRepository.findAllActives(musicianId, startDate, endDate);
        return this.musicianRehearsalEntityListToEventResponseListConverter.convert(rehearsalList);
    }

    @Override
    public void save(final MusicianEventRequest musicianEventRequest) {
        this.musicianRehearsalRepository.save(
                this.musicianEventRequestToMusicianRehearsalEntityConverter.convert(musicianEventRequest)
        );
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
    public List<MusicianEventResponse> findAllActivesMusiciansByRehearsalId(final Long rehearsalId) {
        final List<MusicianRehearsalEntity> rehearsalList = this.musicianRehearsalRepository.findAllActivesMusiciansByRehearsalId(rehearsalId);
        return this.musicianRehearsalEntityListToMusicianEventResponseListConverter.convert(rehearsalList);
    }

}
