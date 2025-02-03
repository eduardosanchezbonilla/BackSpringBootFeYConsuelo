package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSoloist;
import com.feyconsuelo.openapi.model.RepertoireMarchSoloistDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloistDtoToRepertoireMarchSoloistConverter {

    public RepertoireMarchSoloist convert(final RepertoireMarchSoloistDto repertoireMarchSoloistDto) {
        return RepertoireMarchSoloist.builder()
                .musicianId(repertoireMarchSoloistDto.getMusicianId())
                .musicianName(repertoireMarchSoloistDto.getMusicianName())
                .order(repertoireMarchSoloistDto.getOrder())
                .build();
    }

}
