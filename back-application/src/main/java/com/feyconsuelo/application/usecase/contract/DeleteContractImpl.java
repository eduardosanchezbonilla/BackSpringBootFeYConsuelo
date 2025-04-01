package com.feyconsuelo.application.usecase.contract;

import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.usecase.contract.DeleteContract;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteContractImpl implements DeleteContract {

    private final GoogleDriveService googleDriveService;

    @Override
    public void execute(final String contractGoogleId) {
        this.googleDriveService.deleteFile(contractGoogleId);
    }
}
