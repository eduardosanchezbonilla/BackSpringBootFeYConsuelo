package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchEntityListToRepertoireMarchResponseListConverter {

    private final RepertoireMarchEntityToRepertoireMarchResponseConverter repertoireMarchEntityToRepertoireMarchResponseConverter;

    public List<RepertoireMarchResponse> convert(final List<RepertoireMarchEntity> repertoireMarchEntityList) {
        if (CollectionUtils.isEmpty(repertoireMarchEntityList)) {
            return List.of();
        }
        return repertoireMarchEntityList.stream()
                .map(march -> this.repertoireMarchEntityToRepertoireMarchResponseConverter.convert(march, 0, 0))
                .toList();
    }
}
