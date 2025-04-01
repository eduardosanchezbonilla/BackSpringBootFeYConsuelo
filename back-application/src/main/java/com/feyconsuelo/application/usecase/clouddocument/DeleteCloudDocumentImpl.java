package com.feyconsuelo.application.usecase.clouddocument;

import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.usecase.clouddocument.DeleteDocument;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteCloudDocumentImpl implements DeleteDocument {

    private final GoogleDriveService googleDriveService;

    @Override
    public void execute(final String documentGoogleId) {
        this.googleDriveService.deleteFile(documentGoogleId);
    }
}
