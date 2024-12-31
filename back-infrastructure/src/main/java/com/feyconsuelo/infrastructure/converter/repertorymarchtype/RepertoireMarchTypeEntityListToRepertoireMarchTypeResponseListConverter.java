package com.feyconsuelo.infrastructure.converter.repertorymarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.infrastructure.entities.repertoiremarchtype.RepertoireMarchTypeEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchTypeEntityListToRepertoireMarchTypeResponseListConverter {

    private final RepertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter;

    public List<RepertoireMarchTypeResponse> convert(final List<RepertoireMarchTypeEntity> repertoireMarchTypeEntityList) {
        if (CollectionUtils.isEmpty(repertoireMarchTypeEntityList)) {
            return List.of();
        }
        return repertoireMarchTypeEntityList.stream()
                .map(this.repertoireMarchTypeEntityToRepertoireMarchTypeResponseConverter::convert)
                .toList();
    }
}
