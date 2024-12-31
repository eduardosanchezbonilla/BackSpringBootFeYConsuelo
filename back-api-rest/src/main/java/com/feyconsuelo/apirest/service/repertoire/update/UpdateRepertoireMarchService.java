package com.feyconsuelo.apirest.service.repertoire.update;

import com.feyconsuelo.apirest.converter.repertoire.RepertoireMarchRequestDtoToRepertoireMarchRequestConverter;
import com.feyconsuelo.domain.usecase.repertoire.UpdateRepertoireMarch;
import com.feyconsuelo.openapi.model.RepertoireMarchRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateRepertoireMarchService {

    private final UpdateRepertoireMarch updateRepertoireMarch;

    private final RepertoireMarchRequestDtoToRepertoireMarchRequestConverter repertoireMarchRequestDtoToRepertoireMarchRequestConverter;

    public ResponseEntity<Void> updateRepertoireMarch(final Long RepertoireMarchId,
                                                      final RepertoireMarchRequestDto repertoireMarchRequestDto) {
        this.updateRepertoireMarch.execute(
                RepertoireMarchId,
                this.repertoireMarchRequestDtoToRepertoireMarchRequestConverter.convert(repertoireMarchRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
