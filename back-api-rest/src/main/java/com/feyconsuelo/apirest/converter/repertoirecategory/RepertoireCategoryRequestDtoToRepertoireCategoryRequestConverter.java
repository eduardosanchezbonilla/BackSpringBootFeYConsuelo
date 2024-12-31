package com.feyconsuelo.apirest.converter.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryRequest;
import com.feyconsuelo.openapi.model.RepertoireCategoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireCategoryRequestDtoToRepertoireCategoryRequestConverter {

    public RepertoireCategoryRequest convert(final RepertoireCategoryRequestDto repertoireCategoryRequestDto) {
        return RepertoireCategoryRequest.builder()
                .name(repertoireCategoryRequestDto.getName())
                .order(repertoireCategoryRequestDto.getOrder())
                .image(repertoireCategoryRequestDto.getImage())
                .current(repertoireCategoryRequestDto.getCurrent())
                .build();
    }

}
