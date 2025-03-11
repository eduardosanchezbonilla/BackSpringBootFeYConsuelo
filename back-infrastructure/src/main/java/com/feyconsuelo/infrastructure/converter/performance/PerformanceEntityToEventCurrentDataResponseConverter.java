package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCurrentDataResponse;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class PerformanceEntityToEventCurrentDataResponseConverter {

    public EventCurrentDataResponse convert(final PerformanceEntity performanceEntity) {
        return EventCurrentDataResponse.builder()
                .lat(performanceEntity.getCurrentLat())
                .lng(performanceEntity.getCurrentLng())
                .march(performanceEntity.getCurrentMarch())
                .build();
    }
}
