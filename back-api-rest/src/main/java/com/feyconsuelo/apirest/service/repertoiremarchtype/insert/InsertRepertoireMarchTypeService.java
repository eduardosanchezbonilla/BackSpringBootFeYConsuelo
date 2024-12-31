package com.feyconsuelo.apirest.service.repertoiremarchtype.insert;

import com.feyconsuelo.apirest.converter.repertoiremarchtype.RepertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.InsertRepertoireMarchType;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertRepertoireMarchTypeService {

    private final InsertRepertoireMarchType insertRepertoireMarchType;

    private final RepertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter repertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter;

    public ResponseEntity<Void> insertRepertoireMarchType(final RepertoireMarchTypeRequestDto repertoireMarchTypeRequestDto) {
        this.insertRepertoireMarchType.execute(
                this.repertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter.convert(repertoireMarchTypeRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
