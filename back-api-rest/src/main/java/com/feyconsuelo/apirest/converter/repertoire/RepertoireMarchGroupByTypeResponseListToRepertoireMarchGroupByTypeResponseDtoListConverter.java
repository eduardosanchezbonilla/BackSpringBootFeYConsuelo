package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;
import com.feyconsuelo.openapi.model.RepertoireMarchGroupByTypeResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter {

    private final RepertoireMarchGroupByTypeResponseToRepertoireMarchGroupByTypeResponseDtoConverter repertoireMarchGroupByTypeResponseToRepertoireMarchGroupByTypeResponseDtoConverter;

    public List<RepertoireMarchGroupByTypeResponseDto> convert(final List<RepertoireMarchGroupByTypeResponse> repertoireMarchGroupByTypeResponseList) {
        if (CollectionUtils.isEmpty(repertoireMarchGroupByTypeResponseList)) {
            return List.of();
        }
        return repertoireMarchGroupByTypeResponseList.stream()
                .map(this.repertoireMarchGroupByTypeResponseToRepertoireMarchGroupByTypeResponseDtoConverter::convert)
                .sorted(Comparator.comparing(repertoireGroupByType -> repertoireGroupByType.getType().getOrder()))
                .toList();
    }

}
