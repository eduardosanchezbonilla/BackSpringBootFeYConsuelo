package com.feyconsuelo.apirest.service.clouddocument.query;

import com.feyconsuelo.apirest.converter.clouddocument.CloudDocumentResponseToCloudDocumentResponseDtoConverter;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.domain.usecase.clouddocument.DownloadDocument;
import com.feyconsuelo.openapi.model.CloudDocumentResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class DownloadCloudDocumentService {

    private final DownloadDocument downloadDocument;

    private final CloudDocumentResponseToCloudDocumentResponseDtoConverter cloudDocumentResponseToCloudDocumentResponseDtoConverter;

    public ResponseEntity<CloudDocumentResponseDto> downloadDocument(final String documentGoogleId) {
        final Optional<CloudDocumentResponse> cloudDocumentResponse = this.downloadDocument.execute(documentGoogleId);

        return cloudDocumentResponse.map(response -> ResponseEntity.ok(this.cloudDocumentResponseToCloudDocumentResponseDtoConverter.convert(response))).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
