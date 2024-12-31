package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.openapi.model.RepertoireMarchResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchResponseListToRepertoireMarchResponseDtoListConverter {

    private final RepertoireMarchResponseToRepertoireMarchResponseDtoConverter repertoireMarchResponseToRepertoireMarchResponseDtoConverter;

    public List<RepertoireMarchResponseDto> convert(final List<RepertoireMarchResponse> repertoireMarchResponseList) {
        return this.convert(repertoireMarchResponseList, Boolean.TRUE);
    }

    public List<RepertoireMarchResponseDto> convert(final List<RepertoireMarchResponse> repertoireMarchResponseList, final Boolean images) {
        if (CollectionUtils.isEmpty(repertoireMarchResponseList)) {
            return List.of();
        }
        return repertoireMarchResponseList.stream()
                .map(march -> this.repertoireMarchResponseToRepertoireMarchResponseDtoConverter.convert(march, images))
                .sorted(Comparator.comparing(RepertoireMarchResponseDto::getName))
                .toList();
    }

}
