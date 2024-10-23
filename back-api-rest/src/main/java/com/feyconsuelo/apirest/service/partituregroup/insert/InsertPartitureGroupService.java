package com.feyconsuelo.apirest.service.partituregroup.insert;

import com.feyconsuelo.apirest.converter.partituregroup.PartitureGroupRequestDtoToPartitureGroupRequestConverter;
import com.feyconsuelo.domain.usecase.partituregroup.InsertPartitureGroup;
import com.feyconsuelo.openapi.model.PartitureGroupRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertPartitureGroupService {

    private final InsertPartitureGroup insertPartitureGroup;

    private final PartitureGroupRequestDtoToPartitureGroupRequestConverter partitureGroupRequestDtoToPartitureGroupRequestConverter;

    public ResponseEntity<Void> postPartitureGroup(final PartitureGroupRequestDto partitureGroupRequestDto) {
        this.insertPartitureGroup.execute(
                this.partitureGroupRequestDtoToPartitureGroupRequestConverter.convert(partitureGroupRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
