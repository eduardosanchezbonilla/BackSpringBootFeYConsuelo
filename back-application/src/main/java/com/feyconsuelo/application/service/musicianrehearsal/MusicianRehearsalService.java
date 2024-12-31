package com.feyconsuelo.application.service.musicianrehearsal;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventResponse;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface MusicianRehearsalService {

    List<EventResponse> getAll(Long musicianId, LocalDate startDate, LocalDate endDate);

    void save(MusicianEventRequest musicianEventRequest);

    void logicalDelete(Long musicianId, Long eventId);

    List<MusicianEventResponse> findAllActivesMusiciansLastRehearsalUntilDateTime(LocalDateTime dateTime);

    List<MusicianEventResponse> findAllActivesMusiciansByRehearsalId(Long rehearsalId);

}