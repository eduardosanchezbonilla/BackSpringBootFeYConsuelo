package com.feyconsuelo.apirest.service.clouddocument;

import com.feyconsuelo.apirest.service.clouddocument.delete.DeleteCloudDocumentService;
import com.feyconsuelo.apirest.service.clouddocument.insert.UploadCloudDocumentService;
import com.feyconsuelo.apirest.service.clouddocument.query.DownloadCloudDocumentService;
import com.feyconsuelo.apirest.service.clouddocument.query.GetCloudDocumentService;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentRequest;
import com.feyconsuelo.openapi.api.CloudDocumentControllerApiDelegate;
import com.feyconsuelo.openapi.model.CloudDocumentRequestDto;
import com.feyconsuelo.openapi.model.CloudDocumentResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class CloudDocumentApiService implements CloudDocumentControllerApiDelegate {

    private final GetCloudDocumentService getCloudDocumentGroup;
    private final DownloadCloudDocumentService downloadCloudDocumentService;
    private final DeleteCloudDocumentService deleteCloudDocumentService;
    private final UploadCloudDocumentService uploadCloudDocumentService;

    @Override
    public ResponseEntity<List<CloudDocumentResponseDto>> getAllDocumentsInFolder(final String folderGoogleId) {
        return this.getCloudDocumentGroup.getAllDocumentsInFolder(
                CloudDocumentRequest.builder()
                        .folderGoogleId(folderGoogleId)
                        .build()
        );
    }

    @Override
    public ResponseEntity<CloudDocumentResponseDto> downloadDocument(final String documentGoogleId) {
        return this.downloadCloudDocumentService.downloadDocument(documentGoogleId);
    }

    @Override
    public ResponseEntity<Void> deleteDocument(final String documentGoogleId) {
        return this.deleteCloudDocumentService.deleteDocument(documentGoogleId);
    }

    @Override
    public ResponseEntity<CloudDocumentResponseDto> uploadDocument(final CloudDocumentRequestDto cloudDocumentRequestDto) {
        return this.uploadCloudDocumentService.uploadDocument(
                cloudDocumentRequestDto.getName(),
                cloudDocumentRequestDto.getContent(),
                cloudDocumentRequestDto.getMimeType(),
                cloudDocumentRequestDto.getFolderGoogleId()
        );
    }

}
