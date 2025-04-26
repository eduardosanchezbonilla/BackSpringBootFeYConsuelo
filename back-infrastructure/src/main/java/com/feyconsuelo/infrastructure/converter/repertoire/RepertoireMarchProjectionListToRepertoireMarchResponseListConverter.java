package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchProjectionListToRepertoireMarchResponseListConverter {

    private final RepertoireMarchProjectionToRepertoireMarchResponseConverter repertoireMarchProjectionToRepertoireMarchResponseConverter;

    public List<RepertoireMarchResponse> convert(final List<RepertoireMarchProjection> repertoireMarchEntityList, final Boolean returnSolos) {
        if (CollectionUtils.isEmpty(repertoireMarchEntityList)) {
            return List.of();
        }
        return repertoireMarchEntityList.stream()
                .map(march -> this.repertoireMarchProjectionToRepertoireMarchResponseConverter.convert(march, 0, 0, returnSolos))
                .toList();
    }
}
