package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSoloist;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchSecondarySoloistEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSecondarySoloistEntityToRepertoireMarchSoloistConverter {

    public RepertoireMarchSoloist convert(final RepertoireMarchSecondarySoloistEntity entity) {
        return RepertoireMarchSoloist.builder()
                .musicianId(entity.getMusicianId())
                .musicianName(entity.getMusicianName())
                .order(entity.getOrder())
                .build();
    }

}
