package com.feyconsuelo.infrastructure.converter.performance;

import com.feyconsuelo.domain.model.event.EventCrossheadStreet;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadPerformanceEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventCrossheadStreetToCrossheadPerformanceEntityConverter {

    private final EventCrossheadMarchToCrossheadMarchPerformanceEntityConverter eventCrossheadMarchToCrossheadMarchPerformanceEntityConverter;
    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public CrossheadPerformanceEntity convert(final EventCrossheadStreet eventCrossheadStreet, final Long eventId) {
        return CrossheadPerformanceEntity.builder()
                //.id(eventCrossheadStreet.getId())
                .performanceId(eventId)
                .street(eventCrossheadStreet.getStreet())
                .streetOrder(eventCrossheadStreet.getStreetOrder())
                .annotations(eventCrossheadStreet.getAnnotations())
                .marchs(
                        CollectionUtils.isEmpty(eventCrossheadStreet.getMarchs()) ?
                                List.of() :
                                eventCrossheadStreet.getMarchs().stream()
                                        .map(this.eventCrossheadMarchToCrossheadMarchPerformanceEntityConverter::convert)
                                        .toList()
                )
                .modifiedUserCrosshead(this.tokenInfoExtractorService.getUsername())
                .build();
    }

}
