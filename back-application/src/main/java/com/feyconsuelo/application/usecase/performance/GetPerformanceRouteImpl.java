package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.model.event.EventRouteResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetPerformanceRouteImpl {

    private final PerformanceService performanceService;

    public Optional<EventRouteResponse> execute(final Long eventId) {
        return this.performanceService.getRoute(eventId);
    }
}
