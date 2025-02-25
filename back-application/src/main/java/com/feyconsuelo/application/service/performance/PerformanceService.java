package com.feyconsuelo.application.service.performance;

import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventRouteRequest;
import com.feyconsuelo.domain.model.event.LatLng;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface PerformanceService {

    List<EventResponse> getAll(LocalDate startDate, LocalDate endDate);

    Optional<EventResponse> getById(Long eventId, final Boolean isThumbnail, final Boolean rout);

    Optional<EventResponse> getByDate(LocalDate date);

    void insert(EventRequest eventRequest);

    void update(Long eventId, EventRequest eventRequest);

    void delete(Long eventId);

    void logicalDelete(Long eventId);

    void updateFormation(Long eventId, EventFormationRequest eventFormationRequest);

    void updateRoute(final Long eventId, final EventRouteRequest eventRouteRequest);

    void updateCurrentPosition(final Long eventId, final LatLng latLng);
}
