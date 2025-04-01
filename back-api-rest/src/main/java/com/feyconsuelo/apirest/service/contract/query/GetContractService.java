package com.feyconsuelo.apirest.service.contract.query;

import com.feyconsuelo.apirest.converter.contract.ContractResponseListToContractResponseDtoListConverter;
import com.feyconsuelo.domain.model.contract.ContractRequest;
import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.domain.usecase.contract.GetAllContracts;
import com.feyconsuelo.openapi.model.ContractResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetContractService {

    private final GetAllContracts getAllContracts;

    private final ContractResponseListToContractResponseDtoListConverter contractResponseListToContractResponseDtoListConverter;

    public ResponseEntity<List<ContractResponseDto>> getAllContractsInContractGroup(final ContractRequest contractRequest) {
        final List<ContractResponse> contractResponseList = this.getAllContracts.execute(contractRequest);
        if (CollectionUtils.isEmpty(contractResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.contractResponseListToContractResponseDtoListConverter.convert(contractResponseList));
    }

}
