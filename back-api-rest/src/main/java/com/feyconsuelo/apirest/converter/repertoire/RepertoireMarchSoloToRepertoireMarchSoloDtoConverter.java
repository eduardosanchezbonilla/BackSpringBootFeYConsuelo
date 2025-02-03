package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSolo;
import com.feyconsuelo.openapi.model.RepertoireMarchSoloDto;
import com.feyconsuelo.openapi.model.RepertoireMarchSoloistDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloToRepertoireMarchSoloDtoConverter {

    private final RepertoireMarchSoloistToRepertoireMarchSoloistDtoConverter repertoireMarchSoloistToRepertoireMarchSoloistDtoConverter;

    public RepertoireMarchSoloDto convert(final RepertoireMarchSolo repertoireMarchSolo) {
        return RepertoireMarchSoloDto.builder()
                .id(repertoireMarchSolo.getId())
                .name(repertoireMarchSolo.getName())
                .order(repertoireMarchSolo.getOrder())
                .mainSoloists(
                        CollectionUtils.isEmpty(repertoireMarchSolo.getMainSoloists()) ?
                                List.of()
                                :
                                repertoireMarchSolo.getMainSoloists().stream()
                                        .map(this.repertoireMarchSoloistToRepertoireMarchSoloistDtoConverter::convert)
                                        .sorted(Comparator.comparing(RepertoireMarchSoloistDto::getOrder))
                                        .toList()
                )
                .secondarySoloists(
                        CollectionUtils.isEmpty(repertoireMarchSolo.getSecondarySoloists()) ?
                                List.of()
                                :
                                repertoireMarchSolo.getSecondarySoloists().stream()
                                        .map(this.repertoireMarchSoloistToRepertoireMarchSoloistDtoConverter::convert)
                                        .sorted(Comparator.comparing(RepertoireMarchSoloistDto::getOrder))
                                        .toList()
                )
                .build();
    }

}
