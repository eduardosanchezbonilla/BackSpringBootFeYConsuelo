package com.feyconsuelo.application.usecase.contract;

import com.feyconsuelo.application.converter.googledrive.FileResponseToContractResponseConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.contract.ContractRequest;
import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.usecase.contract.UploadContract;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UploadContractImpl implements UploadContract {

    private final GoogleDriveService googleDriveService;
    private final FileResponseToContractResponseConverter fileResponseToContractResponseConverter;

    @Override
    public Optional<ContractResponse> execute(final ContractRequest contractRequest) {
        final Optional<FileResponse> file = this.googleDriveService.uploadFile(
                contractRequest.getContractGroupGoogleId(),
                contractRequest.getName(),
                contractRequest.getContent(),
                contractRequest.getMimeType()
        );
        return file.map(this.fileResponseToContractResponseConverter::convert);
    }
}
