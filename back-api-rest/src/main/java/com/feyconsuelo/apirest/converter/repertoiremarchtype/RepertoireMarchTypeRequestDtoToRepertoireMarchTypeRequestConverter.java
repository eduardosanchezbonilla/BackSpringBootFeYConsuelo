package com.feyconsuelo.apirest.converter.repertoiremarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeRequest;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter {

    public RepertoireMarchTypeRequest convert(final RepertoireMarchTypeRequestDto repertoireMarchTypeRequestDto) {
        return RepertoireMarchTypeRequest.builder()
                .name(repertoireMarchTypeRequestDto.getName())
                .order(repertoireMarchTypeRequestDto.getOrder())
                .image(repertoireMarchTypeRequestDto.getImage())
                .build();
    }

}
