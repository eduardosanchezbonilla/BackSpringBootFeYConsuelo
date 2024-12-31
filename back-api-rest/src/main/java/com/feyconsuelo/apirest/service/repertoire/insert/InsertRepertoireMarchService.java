package com.feyconsuelo.apirest.service.repertoire.insert;

import com.feyconsuelo.apirest.converter.repertoire.RepertoireMarchRequestDtoToRepertoireMarchRequestConverter;
import com.feyconsuelo.domain.usecase.repertoire.InsertRepertoireMarch;
import com.feyconsuelo.openapi.model.RepertoireMarchRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertRepertoireMarchService {

    private final InsertRepertoireMarch insertRepertoireMarch;

    private final RepertoireMarchRequestDtoToRepertoireMarchRequestConverter repertoireMarchRequestDtoToRepertoireMarchRequestConverter;

    public ResponseEntity<Void> insertRepertoireMarch(final RepertoireMarchRequestDto repertoireMarchRequestDto) {
        this.insertRepertoireMarch.execute(
                this.repertoireMarchRequestDtoToRepertoireMarchRequestConverter.convert(repertoireMarchRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
