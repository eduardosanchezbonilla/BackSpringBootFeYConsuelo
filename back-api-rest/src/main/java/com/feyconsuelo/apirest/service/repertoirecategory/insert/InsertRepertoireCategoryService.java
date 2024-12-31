package com.feyconsuelo.apirest.service.repertoirecategory.insert;

import com.feyconsuelo.apirest.converter.repertoirecategory.RepertoireCategoryRequestDtoToRepertoireCategoryRequestConverter;
import com.feyconsuelo.domain.usecase.repertoirecategory.InsertRepertoireCategory;
import com.feyconsuelo.openapi.model.RepertoireCategoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertRepertoireCategoryService {

    private final InsertRepertoireCategory insertRepertoireCategory;

    private final RepertoireCategoryRequestDtoToRepertoireCategoryRequestConverter repertoireCategoryRequestDtoToRepertoireCategoryRequestConverter;

    public ResponseEntity<Void> insertRepertoireCategory(final RepertoireCategoryRequestDto repertoireCategoryRequestDto) {
        this.insertRepertoireCategory.execute(
                this.repertoireCategoryRequestDtoToRepertoireCategoryRequestConverter.convert(repertoireCategoryRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
