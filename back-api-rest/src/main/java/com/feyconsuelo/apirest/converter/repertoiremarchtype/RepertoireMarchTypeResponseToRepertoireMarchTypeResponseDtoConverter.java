package com.feyconsuelo.apirest.converter.repertoiremarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchTypeResponseToRepertoireMarchTypeResponseDtoConverter {

    public RepertoireMarchTypeResponseDto convert(final RepertoireMarchTypeResponse repertoireMarchTypeResponse) {
        return this.convert(repertoireMarchTypeResponse, Boolean.TRUE);
    }

    public RepertoireMarchTypeResponseDto convert(final RepertoireMarchTypeResponse repertoireMarchTypeResponse, final Boolean images) {
        return RepertoireMarchTypeResponseDto.builder()
                .id(repertoireMarchTypeResponse.getId())
                .name(repertoireMarchTypeResponse.getName())
                .order(repertoireMarchTypeResponse.getOrder())
                .image(Boolean.TRUE.equals(images) ? repertoireMarchTypeResponse.getImage() : null)
                .build();
    }

}
