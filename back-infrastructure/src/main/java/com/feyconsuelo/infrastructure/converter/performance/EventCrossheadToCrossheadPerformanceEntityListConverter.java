package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventCrossheadToCrossheadPerformanceEntityListConverter {

    private final EventCrossheadStreetToCrossheadPerformanceEntityConverter eventCrossheadStreetToCrossheadPerformanceEntityConverter;

    public List<CrossheadPerformanceEntity> convert(final EventCrosshead eventCrosshead, final Long eventId) {
        if (CollectionUtils.isEmpty(eventCrosshead.getStreets())) {
            return List.of();
        }
        return eventCrosshead.getStreets().stream()
                .map(street -> this.eventCrossheadStreetToCrossheadPerformanceEntityConverter.convert(street, eventId))
                .toList();
    }
}
