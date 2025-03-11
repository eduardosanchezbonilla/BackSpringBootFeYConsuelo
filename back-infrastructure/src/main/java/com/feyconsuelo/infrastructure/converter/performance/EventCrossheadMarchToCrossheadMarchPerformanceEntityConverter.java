package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCrossheadMarch;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadMarchPerformanceEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventCrossheadMarchToCrossheadMarchPerformanceEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public CrossheadMarchPerformanceEntity convert(final EventCrossheadMarch eventCrossheadMarch) {
        return CrossheadMarchPerformanceEntity.builder()
                //.id(eventCrossheadMarch.getId())
                .marchId(eventCrossheadMarch.getMarchId())
                .marchName(eventCrossheadMarch.getMarchName())
                .marchOrder(eventCrossheadMarch.getMarchOrder())
                .annotations(eventCrossheadMarch.getAnnotations())
                .modifiedUserCrossheadMarch(this.tokenInfoExtractorService.getUsername())
                .build();
    }

}
