package com.feyconsuelo.apirest.service.repertoirecategory.update;

import com.feyconsuelo.apirest.converter.repertoirecategory.RepertoireCategoryRequestDtoToRepertoireCategoryRequestConverter;
import com.feyconsuelo.domain.usecase.repertoirecategory.UpdateRepertoireCategory;
import com.feyconsuelo.openapi.model.RepertoireCategoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateRepertoireCategoryService {

    private final UpdateRepertoireCategory updateRepertoireCategory;

    private final RepertoireCategoryRequestDtoToRepertoireCategoryRequestConverter repertoireCategoryRequestDtoToRepertoireCategoryRequestConverter;

    public ResponseEntity<Void> updateRepertoireCategory(final Long RepertoireCategoryId,
                                                         final RepertoireCategoryRequestDto repertoireCategoryRequestDto) {
        this.updateRepertoireCategory.execute(
                RepertoireCategoryId,
                this.repertoireCategoryRequestDtoToRepertoireCategoryRequestConverter.convert(repertoireCategoryRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
