package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSolo;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchSoloEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloEntityToRepertoireMarchSoloConverter {

    private final RepertoireMarchMainSoloistEntityToRepertoireMarchSoloistConverter repertoireMarchMainSoloistEntityToRepertoireMarchSoloistConverter;
    private final RepertoireMarchSecondarySoloistEntityToRepertoireMarchSoloistConverter repertoireMarchSecondarySoloistEntityToRepertoireMarchSoloistConverter;

    public RepertoireMarchSolo convert(final RepertoireMarchSoloEntity entity) {
        return RepertoireMarchSolo.builder()
                .id(entity.getId())
                .name(entity.getName())
                .order(entity.getOrder())
                .mainSoloists(
                        CollectionUtils.isEmpty(entity.getMainSoloists()) ?
                                null :
                                entity.getMainSoloists().stream()
                                        .map(this.repertoireMarchMainSoloistEntityToRepertoireMarchSoloistConverter::convert)
                                        .toList()
                )
                .secondarySoloists(
                        CollectionUtils.isEmpty(entity.getSecondarySoloists()) ?
                                null :
                                entity.getSecondarySoloists().stream()
                                        .map(this.repertoireMarchSecondarySoloistEntityToRepertoireMarchSoloistConverter::convert)
                                        .toList()
                )
                .build();
    }

}
