package com.feyconsuelo.apirest.service.repertoiremarchtype.update;

import com.feyconsuelo.apirest.converter.repertoiremarchtype.RepertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.UpdateRepertoireMarchType;
import com.feyconsuelo.openapi.model.RepertoireMarchTypeRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateRepertoireMarchTypeService {

    private final UpdateRepertoireMarchType updateRepertoireMarchType;

    private final RepertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter repertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter;

    public ResponseEntity<Void> updateRepertoireMarchType(final Long RepertoireMarchTypeId,
                                                          final RepertoireMarchTypeRequestDto repertoireMarchTypeRequestDto) {
        this.updateRepertoireMarchType.execute(
                RepertoireMarchTypeId,
                this.repertoireMarchTypeRequestDtoToRepertoireMarchTypeRequestConverter.convert(repertoireMarchTypeRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
