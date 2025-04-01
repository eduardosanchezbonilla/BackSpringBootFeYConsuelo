package com.feyconsuelo.application.usecase.contract;

import com.feyconsuelo.application.converter.googledrive.FileResponseToContractResponseConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.contract.ContractResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.usecase.contract.DownloadContract;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class DownloadContractImpl implements DownloadContract {

    private final GoogleDriveService googleDriveService;
    private final FileResponseToContractResponseConverter fileResponseToContractResponseConverter;

    @Override
    public Optional<ContractResponse> execute(final String contractGoogleId) {
        final Optional<FileResponse> file = this.googleDriveService.downloadFile(contractGoogleId);
        return file.map(this.fileResponseToContractResponseConverter::convert);
    }
}
