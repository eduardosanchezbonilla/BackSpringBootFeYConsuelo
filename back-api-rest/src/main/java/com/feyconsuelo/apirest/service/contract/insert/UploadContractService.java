package com.feyconsuelo.apirest.service.contract.insert;

import com.feyconsuelo.apirest.converter.contract.ContractResponseToContractResponseDtoConverter;
import com.feyconsuelo.domain.model.contract.ContractRequest;
import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.domain.usecase.contract.UploadContract;
import com.feyconsuelo.openapi.model.ContractResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class UploadContractService {

    private final UploadContract uploadContract;

    private final ContractResponseToContractResponseDtoConverter contractResponseToContractResponseDtoConverter;

    public ResponseEntity<ContractResponseDto> uploadContract(final String name, final String content, final String mimeType, final String folderGoogleId) {
        final Optional<ContractResponse> contractResponse = this.uploadContract.execute(
                ContractRequest.builder()
                        .name(name)
                        .content(content)
                        .mimeType(mimeType)
                        .contractGroupGoogleId(folderGoogleId)
                        .build()
        );

        return contractResponse.map(response -> ResponseEntity.ok(this.contractResponseToContractResponseDtoConverter.convert(response))).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
