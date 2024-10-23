package com.feyconsuelo.apirest.service.userpatituregroup.insert;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupRequest;
import com.feyconsuelo.domain.usecase.userpartituregroup.InsertUserPartitureGroup;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertUserPartitureGroupService {

    private final InsertUserPartitureGroup insertUserPartitureGroup;

    public ResponseEntity<Void> insertUserPartitureGroup(final String username,
                                                         final Long partitureGroupId) {
        this.insertUserPartitureGroup.execute(
                UserPartitureGroupRequest.builder()
                        .username(username.toLowerCase())
                        .partitureGroupId(partitureGroupId)
                        .build()
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
