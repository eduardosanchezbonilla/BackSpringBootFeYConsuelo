package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCrossheadMarch;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadMarchPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class CrossheadMarchPerformanceEntityToEventCrossheadMarchConverter {

    public EventCrossheadMarch convert(final CrossheadMarchPerformanceEntity crossheadMarchPerformanceEntity) {
        return EventCrossheadMarch.builder()
                .id(crossheadMarchPerformanceEntity.getId())
                .marchId(crossheadMarchPerformanceEntity.getMarchId())
                .marchName(crossheadMarchPerformanceEntity.getMarchName())
                .marchOrder(crossheadMarchPerformanceEntity.getMarchOrder())
                .annotations(crossheadMarchPerformanceEntity.getAnnotations())
                .build();
    }

}
