package com.feyconsuelo.apirest.service.clouddocument.insert;

import com.feyconsuelo.apirest.converter.clouddocument.CloudDocumentResponseToCloudDocumentResponseDtoConverter;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentRequest;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.domain.usecase.clouddocument.UploadDocument;
import com.feyconsuelo.openapi.model.CloudDocumentResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class UploadCloudDocumentService {

    private final UploadDocument uploadDocument;

    private final CloudDocumentResponseToCloudDocumentResponseDtoConverter cloudDocumentResponseToCloudDocumentResponseDtoConverter;

    public ResponseEntity<CloudDocumentResponseDto> uploadDocument(final String name, final String content, final String mimeType, final String folderGoogleId) {
        final Optional<CloudDocumentResponse> cloudDocumentResponse = this.uploadDocument.execute(
                CloudDocumentRequest.builder()
                        .name(name)
                        .content(content)
                        .mimeType(mimeType)
                        .folderGoogleId(folderGoogleId)
                        .build()
        );

        return cloudDocumentResponse.map(response -> ResponseEntity.ok(this.cloudDocumentResponseToCloudDocumentResponseDtoConverter.convert(response))).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
