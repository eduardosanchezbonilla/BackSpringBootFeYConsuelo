package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventRouteResponse;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class PerformanceEntityToEventRouteResponseConverter {

    private final EventRouteStringToEventRouteResponseConverter eventRouteStringToEventRouteResponseConverter;

    public EventRouteResponse convert(final PerformanceEntity performanceEntity) {
        return this.eventRouteStringToEventRouteResponseConverter.convert(performanceEntity.getRoute());
    }
}
