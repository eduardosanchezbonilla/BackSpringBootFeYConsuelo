package com.feyconsuelo.apirest.service.partituregroup.delete;

import com.feyconsuelo.domain.usecase.partituregroup.DeletePartitureGroup;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeletePartitureGroupService {

    private final DeletePartitureGroup deletePartitureGroup;

    public ResponseEntity<Void> deletePartitureGroup(final Long partitureGroupId) {
        this.deletePartitureGroup.execute(partitureGroupId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
