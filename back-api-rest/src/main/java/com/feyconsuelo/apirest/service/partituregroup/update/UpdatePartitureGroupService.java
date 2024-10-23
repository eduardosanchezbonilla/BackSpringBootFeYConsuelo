package com.feyconsuelo.apirest.service.partituregroup.update;

import com.feyconsuelo.apirest.converter.partituregroup.PartitureGroupRequestDtoToPartitureGroupRequestConverter;
import com.feyconsuelo.apirest.converter.partituregroup.PartitureGroupResponseListToPartitureGroupResponseDtoListConverter;
import com.feyconsuelo.domain.usecase.partituregroup.UpdatePartitureGroup;
import com.feyconsuelo.openapi.model.PartitureGroupRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdatePartitureGroupService {

    private final UpdatePartitureGroup updatePartitureGroup;

    private final PartitureGroupRequestDtoToPartitureGroupRequestConverter partitureGroupRequestDtoToPartitureGroupRequestConverter;

    private final PartitureGroupResponseListToPartitureGroupResponseDtoListConverter partitureGroupResponseListToPartitureGroupResponseDtoListConverter;

    public ResponseEntity<Void> updatePartitureGroup(final Long partitureGroupId,
                                                     final PartitureGroupRequestDto partitureGroupRequestDto) {
        this.updatePartitureGroup.execute(
                partitureGroupId,
                this.partitureGroupRequestDtoToPartitureGroupRequestConverter.convert(partitureGroupRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
