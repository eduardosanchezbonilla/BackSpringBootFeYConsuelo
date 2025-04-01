package com.feyconsuelo.apirest.service.contractgroup.insert;

import com.feyconsuelo.apirest.converter.contractgroup.ContractGroupRequestDtoToContractGroupRequestConverter;
import com.feyconsuelo.domain.usecase.contractgroup.InsertContractGroup;
import com.feyconsuelo.openapi.model.ContractGroupRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertContractGroupService {

    private final InsertContractGroup insertContractGroup;

    private final ContractGroupRequestDtoToContractGroupRequestConverter contractGroupRequestDtoToContractGroupRequestConverter;

    public ResponseEntity<Void> postContractGroup(final ContractGroupRequestDto contractGroupRequestDto) {
        this.insertContractGroup.execute(
                this.contractGroupRequestDtoToContractGroupRequestConverter.convert(contractGroupRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
