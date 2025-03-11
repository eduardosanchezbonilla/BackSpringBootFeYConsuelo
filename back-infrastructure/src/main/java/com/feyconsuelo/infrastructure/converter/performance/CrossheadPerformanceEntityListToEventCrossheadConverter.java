package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class CrossheadPerformanceEntityListToEventCrossheadConverter {

    private final CrossheadPerformanceEntityToEventCrossheadStreetConverter crossheadPerformanceEntityToEventCrossheadStreetConverter;

    public EventCrosshead convert(final List<CrossheadPerformanceEntity> crossheadPerformanceEntityList) {
        return EventCrosshead.builder()
                .streets(crossheadPerformanceEntityList.stream()
                        .map(this.crossheadPerformanceEntityToEventCrossheadStreetConverter::convert)
                        .toList())
                .build();
    }
}
