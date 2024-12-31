package com.feyconsuelo.apirest.converter.repertoire;

import com.feyconsuelo.apirest.converter.repertoiremarchtype.RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;
import com.feyconsuelo.openapi.model.RepertoireMarchGroupByTypeResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchGroupByTypeResponseToRepertoireMarchGroupByTypeResponseDtoConverter {

    private final RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter;
    private final RepertoireMarchResponseListToRepertoireMarchResponseDtoListConverter repertoireMarchResponseListToRepertoireMarchResponseDtoListConverter;

    public RepertoireMarchGroupByTypeResponseDto convert(final RepertoireMarchGroupByTypeResponse repertoireMarchGroupByTypeResponse) {
        return RepertoireMarchGroupByTypeResponseDto.builder()
                .type(this.repertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter.convert(repertoireMarchGroupByTypeResponse.getType()))
                .marchs(this.repertoireMarchResponseListToRepertoireMarchResponseDtoListConverter.convert(repertoireMarchGroupByTypeResponse.getMarchs(), Boolean.FALSE))
                .build();
    }

}
