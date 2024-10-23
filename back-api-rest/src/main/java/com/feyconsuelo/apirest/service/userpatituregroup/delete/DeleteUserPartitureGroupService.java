package com.feyconsuelo.apirest.service.userpatituregroup.delete;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupRequest;
import com.feyconsuelo.domain.usecase.userpartituregroup.DeleteUserPartitureGroup;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteUserPartitureGroupService {

    private final DeleteUserPartitureGroup deleteUserPartitureGroup;

    public ResponseEntity<Void> deleteUserPartitureGroup(final String username,
                                                         final Long partitureGroupId) {
        this.deleteUserPartitureGroup.execute(
                UserPartitureGroupRequest.builder()
                        .username(username.toLowerCase())
                        .partitureGroupId(partitureGroupId)
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
