package com.feyconsuelo.apirest.converter.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.openapi.model.RepertoireCategoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireCategoryResponseToRepertoireCategoryResponseDtoConverter {

    public RepertoireCategoryResponseDto convert(final RepertoireCategoryResponse repertoireCategoryResponse) {
        return this.convert(repertoireCategoryResponse, Boolean.TRUE);
    }

    public RepertoireCategoryResponseDto convert(final RepertoireCategoryResponse repertoireCategoryResponse, final Boolean images) {
        return RepertoireCategoryResponseDto.builder()
                .id(repertoireCategoryResponse.getId())
                .name(repertoireCategoryResponse.getName())
                .order(repertoireCategoryResponse.getOrder())
                .image(Boolean.TRUE.equals(images) ? repertoireCategoryResponse.getImage() : null)
                .current(repertoireCategoryResponse.getCurrent())
                .build();
    }

}
