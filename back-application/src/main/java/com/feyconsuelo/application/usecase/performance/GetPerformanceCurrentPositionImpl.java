package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.model.event.LatLng;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetPerformanceCurrentPositionImpl {

    private final PerformanceService performanceService;

    public Optional<LatLng> execute(final Long eventId) {
        return this.performanceService.getCurrentPosition(eventId);
    }
}
