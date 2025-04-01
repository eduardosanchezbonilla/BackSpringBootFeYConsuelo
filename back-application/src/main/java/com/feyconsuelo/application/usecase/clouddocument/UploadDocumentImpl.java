package com.feyconsuelo.application.usecase.clouddocument;

import com.feyconsuelo.application.converter.googledrive.FileResponseToCloudDocumentResponseConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentRequest;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.usecase.clouddocument.UploadDocument;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UploadDocumentImpl implements UploadDocument {

    private final GoogleDriveService googleDriveService;
    private final FileResponseToCloudDocumentResponseConverter fileResponseToCloudDocumentResponseConverter;

    @Override
    public Optional<CloudDocumentResponse> execute(final CloudDocumentRequest cloudDocumentRequest) {
        final Optional<FileResponse> file = this.googleDriveService.uploadFile(
                cloudDocumentRequest.getFolderGoogleId(),
                cloudDocumentRequest.getName(),
                cloudDocumentRequest.getContent(),
                cloudDocumentRequest.getMimeType()
        );
        return file.map(this.fileResponseToCloudDocumentResponseConverter::convert);
    }
}
