package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetPerformanceImpl {

    private final PerformanceService performanceService;

    public Optional<EventResponse> execute(final Long eventId, final Boolean isThumbnail, final Boolean route) {
        return this.performanceService.getById(eventId, isThumbnail, route);
    }
}
