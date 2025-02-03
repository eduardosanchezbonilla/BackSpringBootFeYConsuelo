package com.feyconsuelo.application.service.rehearsal;

import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface RehearsalService {

    List<EventResponse> getAll(LocalDate startDate, LocalDate endDate);

    Optional<EventResponse> getById(Long eventId);

    Optional<EventResponse> getByDate(LocalDate date);

    void insert(EventRequest eventRequest);

    void update(Long eventId, EventRequest eventRequest);

    void delete(Long eventId);

    void logicalDelete(Long eventId);

    Optional<EventResponse> findLastRehearsalUntilDateTime(final LocalDateTime dateTime);

    void updateFormation(Long eventId, EventFormationRequest eventFormationRequest);
}
