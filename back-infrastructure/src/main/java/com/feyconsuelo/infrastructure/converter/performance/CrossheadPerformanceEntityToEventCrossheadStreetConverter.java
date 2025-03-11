package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCrossheadStreet;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadPerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class CrossheadPerformanceEntityToEventCrossheadStreetConverter {

    private final CrossheadMarchPerformanceEntityToEventCrossheadMarchConverter crossheadMarchPerformanceEntityToEventCrossheadMarchConverter;

    public EventCrossheadStreet convert(final CrossheadPerformanceEntity crossheadPerformanceEntity) {
        return EventCrossheadStreet.builder()
                .id(crossheadPerformanceEntity.getId())
                .street(crossheadPerformanceEntity.getStreet())
                .streetOrder(crossheadPerformanceEntity.getStreetOrder())
                .annotations(crossheadPerformanceEntity.getAnnotations())
                .marchs(
                        CollectionUtils.isEmpty(crossheadPerformanceEntity.getMarchs()) ?
                                List.of() :
                                crossheadPerformanceEntity.getMarchs().stream()
                                        .map(this.crossheadMarchPerformanceEntityToEventCrossheadMarchConverter::convert)
                                        .toList()
                )
                .build();
    }

}
