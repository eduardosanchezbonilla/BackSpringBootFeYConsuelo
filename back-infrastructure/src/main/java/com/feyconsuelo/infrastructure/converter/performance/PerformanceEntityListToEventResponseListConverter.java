package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class PerformanceEntityListToEventResponseListConverter {

    private final PerformanceEntityToEventResponseConverter performanceEntityToEventResponseConverter;

    public List<EventResponse> convert(final List<PerformanceEntity> performanceEntityList) {
        if (CollectionUtils.isEmpty(performanceEntityList)) {
            return List.of();
        }
        return performanceEntityList.stream()
                .map(this.performanceEntityToEventResponseConverter::convert)
                .toList();
    }
}
