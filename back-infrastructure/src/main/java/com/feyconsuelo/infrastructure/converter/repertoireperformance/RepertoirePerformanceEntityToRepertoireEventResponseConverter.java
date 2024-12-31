package com.feyconsuelo.infrastructure.converter.repertoireperformance;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchEntityToRepertoireMarchResponseConverter;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformanceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoirePerformanceEntityToRepertoireEventResponseConverter {

    private final RepertoireMarchEntityToRepertoireMarchResponseConverter repertoireMarchEntityToRepertoireMarchResponseConverter;
    private final RepertoirePerformanceEntityToEventResponseConverter repertoirePerformanceEntityToEventResponseConverter;

    public RepertoireEventResponse convert(final RepertoirePerformanceEntity performanceEntity) {
        final RepertoireMarchResponse march = this.repertoireMarchEntityToRepertoireMarchResponseConverter.convert(performanceEntity.getMarch());
        final EventResponse event = this.repertoirePerformanceEntityToEventResponseConverter.convert(performanceEntity);

        return RepertoireEventResponse.builder()
                .repertoireMarchResponse(march)
                .eventResponse(event)
                .build();
    }
}
