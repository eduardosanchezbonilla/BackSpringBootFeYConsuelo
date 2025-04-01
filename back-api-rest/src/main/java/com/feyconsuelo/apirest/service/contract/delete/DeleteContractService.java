package com.feyconsuelo.apirest.service.contract.delete;

import com.feyconsuelo.domain.usecase.contract.DeleteContract;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteContractService {

    private final DeleteContract deleteContract;

    public ResponseEntity<Void> deleteContract(final String contractGoogleId) {
        this.deleteContract.execute(contractGoogleId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
