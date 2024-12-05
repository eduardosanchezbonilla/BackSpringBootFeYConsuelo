package com.feyconsuelo.application.service.musicianperformance;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventRequest;

import java.time.LocalDate;
import java.util.List;

public interface MusicianPerformanceService {

    List<EventResponse> getAll(Long musicianId, LocalDate startDate, LocalDate endDate);

    void save(MusicianEventRequest musicianEventRequest);

    void logicalDelete(Long musicianId, Long eventId);
}
