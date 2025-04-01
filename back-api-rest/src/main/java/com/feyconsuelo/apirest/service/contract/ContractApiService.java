package com.feyconsuelo.apirest.service.contract;

import com.feyconsuelo.apirest.service.contract.delete.DeleteContractService;
import com.feyconsuelo.apirest.service.contract.insert.UploadContractService;
import com.feyconsuelo.apirest.service.contract.query.DownloadContractService;
import com.feyconsuelo.apirest.service.contract.query.GetContractService;
import com.feyconsuelo.domain.model.contract.ContractRequest;
import com.feyconsuelo.openapi.api.ContractControllerApiDelegate;
import com.feyconsuelo.openapi.model.ContractRequestDto;
import com.feyconsuelo.openapi.model.ContractResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class ContractApiService implements ContractControllerApiDelegate {

    private final GetContractService getContractGroup;
    private final DownloadContractService downloadContractService;
    private final DeleteContractService deleteContractService;
    private final UploadContractService uploadContractService;

    @Override
    public ResponseEntity<List<ContractResponseDto>> getAllContractsInContractGroup(final String contractGroupGoogleId) {
        return this.getContractGroup.getAllContractsInContractGroup(
                ContractRequest.builder()
                        .contractGroupGoogleId(contractGroupGoogleId)
                        .build()
        );
    }

    @Override
    public ResponseEntity<ContractResponseDto> downloadContract(final String contractGoogleId) {
        return this.downloadContractService.downloadContract(contractGoogleId);
    }

    @Override
    public ResponseEntity<Void> deleteContract(String contractGoogleId) {
        return this.deleteContractService.deleteContract(contractGoogleId);
    }

    @Override
    public ResponseEntity<ContractResponseDto> uploadContract(ContractRequestDto contractRequestDto) {
        return this.uploadContractService.uploadContract(
                contractRequestDto.getName(),
                contractRequestDto.getContent(),
                contractRequestDto.getMimeType(),
                contractRequestDto.getFolderGoogleId()
        );
    }

}
