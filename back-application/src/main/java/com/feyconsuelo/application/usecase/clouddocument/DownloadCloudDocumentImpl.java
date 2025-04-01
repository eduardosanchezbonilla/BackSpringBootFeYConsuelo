package com.feyconsuelo.application.usecase.clouddocument;

import com.feyconsuelo.application.converter.googledrive.FileResponseToCloudDocumentResponseConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.usecase.clouddocument.DownloadDocument;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class DownloadCloudDocumentImpl implements DownloadDocument {

    private final GoogleDriveService googleDriveService;
    private final FileResponseToCloudDocumentResponseConverter fileResponseToCloudDocumentResponseConverter;

    @Override
    public Optional<CloudDocumentResponse> execute(final String documentGoogleId) {
        final Optional<FileResponse> file = this.googleDriveService.downloadFile(documentGoogleId);
        return file.map(this.fileResponseToCloudDocumentResponseConverter::convert);
    }
}
