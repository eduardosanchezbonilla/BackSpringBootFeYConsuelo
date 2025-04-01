package com.feyconsuelo.application.usecase.clouddocument;

import com.feyconsuelo.application.converter.googledrive.FileResponseListToCloudDocumentResponseListConverter;
import com.feyconsuelo.application.service.googledrive.GoogleDriveService;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentRequest;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.domain.model.googledrive.FileResponse;
import com.feyconsuelo.domain.usecase.clouddocument.GetAllDocuments;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllCloudDocumentImpl implements GetAllDocuments {

    private final GoogleDriveService googleDriveService;
    private final FileResponseListToCloudDocumentResponseListConverter fileResponseListToCloudDocumentResponseListConverter;

    @Override
    public List<CloudDocumentResponse> execute(final CloudDocumentRequest cloudDocumentRequest) {
        final List<FileResponse> files = this.googleDriveService.getAllFilesInDirectory(cloudDocumentRequest.getFolderGoogleId());

        return this.fileResponseListToCloudDocumentResponseListConverter.convert(files);
    }
}
