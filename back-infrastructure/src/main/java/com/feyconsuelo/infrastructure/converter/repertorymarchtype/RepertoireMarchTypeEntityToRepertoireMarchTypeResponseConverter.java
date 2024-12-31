package com.feyconsuelo.infrastructure.converter.repertorymarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.infrastructure.entities.repertoiremarchtype.RepertoireMarchTypeEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter {

    public RepertoireMarchTypeResponse convert(final RepertoireMarchTypeEntity repertoireMarchTypeEntity) {
        return RepertoireMarchTypeResponse.builder()
                .id(repertoireMarchTypeEntity.getId())
                .name(repertoireMarchTypeEntity.getName())
                .order(repertoireMarchTypeEntity.getOrder())
                .image(repertoireMarchTypeEntity.getImage())
                .deleteDate(repertoireMarchTypeEntity.getRepertoireMarchTypeDeleteDate())
                .build();
    }

}
