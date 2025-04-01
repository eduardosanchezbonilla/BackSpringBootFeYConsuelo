package com.feyconsuelo.apirest.service.contractgroup.update;

import com.feyconsuelo.apirest.converter.contractgroup.ContractGroupRequestDtoToContractGroupRequestConverter;
import com.feyconsuelo.domain.usecase.contractgroup.UpdateContractGroup;
import com.feyconsuelo.openapi.model.ContractGroupRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateContractGroupService {

    private final UpdateContractGroup updateContractGroup;

    private final ContractGroupRequestDtoToContractGroupRequestConverter contractGroupRequestDtoToContractGroupRequestConverter;

    public ResponseEntity<Void> updateContractGroup(final Long ContractGroupId,
                                                    final ContractGroupRequestDto contractGroupRequestDto) {
        this.updateContractGroup.execute(
                ContractGroupId,
                this.contractGroupRequestDtoToContractGroupRequestConverter.convert(contractGroupRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
