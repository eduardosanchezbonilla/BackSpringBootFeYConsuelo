package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.model.event.EventCurrentDataResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetPerformanceCurrentDataImpl {

    private final PerformanceService performanceService;

    public Optional<EventCurrentDataResponse> execute(final Long eventId) {
        return this.performanceService.getCurrentData(eventId);
    }
}
