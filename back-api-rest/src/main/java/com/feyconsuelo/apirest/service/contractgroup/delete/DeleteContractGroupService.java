package com.feyconsuelo.apirest.service.contractgroup.delete;

import com.feyconsuelo.domain.usecase.contractgroup.DeleteContractGroup;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteContractGroupService {

    private final DeleteContractGroup deleteContractGroup;

    public ResponseEntity<Void> deleteContractGroup(final Long contractGroupId) {
        this.deleteContractGroup.execute(contractGroupId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
