package com.feyconsuelo.infrastructure.converter.repertoirerehearsal;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;
import com.feyconsuelo.infrastructure.converter.repertoire.RepertoireMarchEntityToRepertoireMarchResponseConverter;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireRehearsalEntityToRepertoireEventResponseConverter {

    private final RepertoireMarchEntityToRepertoireMarchResponseConverter repertoireMarchEntityToRepertoireMarchResponseConverter;
    private final RepertoireRehearsalEntityToEventResponseConverter repertoireRehearsalEntityToEventResponseConverter;

    public RepertoireEventResponse convert(final RepertoireRehearsalEntity rehearsalEntity) {
        final RepertoireMarchResponse march = this.repertoireMarchEntityToRepertoireMarchResponseConverter.convert(rehearsalEntity.getMarch());
        final EventResponse event = this.repertoireRehearsalEntityToEventResponseConverter.convert(rehearsalEntity);

        return RepertoireEventResponse.builder()
                .repertoireMarchResponse(march)
                .eventResponse(event)
                .build();
    }
}
