package com.feyconsuelo.apirest.service.contract.query;

import com.feyconsuelo.apirest.converter.contract.ContractResponseToContractResponseDtoConverter;
import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.domain.usecase.contract.DownloadContract;
import com.feyconsuelo.openapi.model.ContractResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class DownloadContractService {

    private final DownloadContract downloadContract;

    private final ContractResponseToContractResponseDtoConverter contractResponseToContractResponseDtoConverter;

    public ResponseEntity<ContractResponseDto> downloadContract(final String contractGoogleId) {
        final Optional<ContractResponse> contractResponse = this.downloadContract.execute(contractGoogleId);

        return contractResponse.map(response -> ResponseEntity.ok(this.contractResponseToContractResponseDtoConverter.convert(response))).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
