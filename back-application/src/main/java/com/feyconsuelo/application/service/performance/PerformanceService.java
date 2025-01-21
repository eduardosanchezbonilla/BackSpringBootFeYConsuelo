package com.feyconsuelo.application.service.performance;

import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface PerformanceService {

    List<EventResponse> getAll(LocalDate startDate, LocalDate endDate);

    Optional<EventResponse> getById(Long eventId, final Boolean isThumbnail);

    Optional<EventResponse> getByDate(LocalDate date);

    void insert(EventRequest eventRequest);

    void update(Long eventId, EventRequest eventRequest);

    void delete(Long eventId);

    void logicalDelete(Long eventId);
}
