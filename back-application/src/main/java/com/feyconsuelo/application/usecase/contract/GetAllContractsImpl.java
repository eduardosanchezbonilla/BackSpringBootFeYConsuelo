package com.feyconsuelo.application.usecase.contract;

import com.feyconsuelo.application.converter.googledrive.FileResponseListToContractResponseListConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.contract.ContractRequest;
import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.usecase.contract.GetAllContracts;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllContractsImpl implements GetAllContracts {

    private final GoogleDriveService googleDriveService;
    private final FileResponseListToContractResponseListConverter fileResponseListToContractResponseListConverter;

    @Override
    public List<ContractResponse> execute(final ContractRequest contractRequest) {
        final List<FileResponse> files = this.googleDriveService.getAllFilesInDirectory(contractRequest.getContractGroupGoogleId());

        return this.fileResponseListToContractResponseListConverter.convert(files);
    }
}
