package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSolo;
import com.feyconsuelo.openapi.model.RepertoireMarchSoloDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloDtoToRepertoireMarchSoloConverter {

    private final RepertoireMarchSoloistDtoToRepertoireMarchSoloistConverter repertoireMarchSoloistDtoToRepertoireMarchSoloistConverter;

    public RepertoireMarchSolo convert(final RepertoireMarchSoloDto repertoireMarchSoloDto) {
        return RepertoireMarchSolo.builder()
                .id(repertoireMarchSoloDto.getId())
                .name(repertoireMarchSoloDto.getName())
                .order(repertoireMarchSoloDto.getOrder())
                .mainSoloists(
                        CollectionUtils.isEmpty(repertoireMarchSoloDto.getMainSoloists()) ?
                                List.of()
                                :
                                repertoireMarchSoloDto.getMainSoloists().stream()
                                        .map(this.repertoireMarchSoloistDtoToRepertoireMarchSoloistConverter::convert)
                                        .toList()
                )
                .secondarySoloists(
                        CollectionUtils.isEmpty(repertoireMarchSoloDto.getSecondarySoloists()) ?
                                List.of()
                                :
                                repertoireMarchSoloDto.getSecondarySoloists().stream()
                                        .map(this.repertoireMarchSoloistDtoToRepertoireMarchSoloistConverter::convert)
                                        .toList()
                )
                .build();
    }

}
