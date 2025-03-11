package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class PerformanceEntityToLatLngConverter {

    public LatLng convert(final PerformanceEntity performanceEntity) {
        return LatLng.builder()
                .lat(performanceEntity.getCurrentLat())
                .lng(performanceEntity.getCurrentLng())
                .build();
    }
}
