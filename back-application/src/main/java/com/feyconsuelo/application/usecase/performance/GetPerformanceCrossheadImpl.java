package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.model.event.EventCrosshead;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetPerformanceCrossheadImpl {

    private final PerformanceService performanceService;

    public Optional<EventCrosshead> execute(final Long eventId) {
        return this.performanceService.getCrosshead(eventId);
    }
}
