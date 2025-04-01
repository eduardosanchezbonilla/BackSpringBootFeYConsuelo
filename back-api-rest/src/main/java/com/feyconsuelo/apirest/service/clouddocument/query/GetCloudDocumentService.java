package com.feyconsuelo.apirest.service.clouddocument.query;

import com.feyconsuelo.apirest.converter.clouddocument.CloudDocumentResponseListToCloudDocumentResponseDtoListConverter;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentRequest;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;
import com.feyconsuelo.domain.usecase.clouddocument.GetAllDocuments;
import com.feyconsuelo.openapi.model.CloudDocumentResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetCloudDocumentService {

    private final GetAllDocuments getAllDocuments;

    private final CloudDocumentResponseListToCloudDocumentResponseDtoListConverter cloudDocumentResponseListToCloudDocumentResponseDtoListConverter;

    public ResponseEntity<List<CloudDocumentResponseDto>> getAllDocumentsInFolder(final CloudDocumentRequest cloudDocumentRequest) {
        final List<CloudDocumentResponse> documentResponseList = this.getAllDocuments.execute(cloudDocumentRequest);
        if (CollectionUtils.isEmpty(documentResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.cloudDocumentResponseListToCloudDocumentResponseDtoListConverter.convert(documentResponseList));
    }

}
