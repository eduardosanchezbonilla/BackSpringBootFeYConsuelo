package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSoloist;
import com.feyconsuelo.openapi.model.RepertoireMarchSoloistDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloistToRepertoireMarchSoloistDtoConverter {

    public RepertoireMarchSoloistDto convert(final RepertoireMarchSoloist repertoireMarchSoloist) {
        return RepertoireMarchSoloistDto.builder()
                .musicianId(repertoireMarchSoloist.getMusicianId())
                .musicianName(repertoireMarchSoloist.getMusicianName())
                .order(repertoireMarchSoloist.getOrder())
                .build();
    }

}
