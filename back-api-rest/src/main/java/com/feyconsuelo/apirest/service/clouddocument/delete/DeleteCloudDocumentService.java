package com.feyconsuelo.apirest.service.clouddocument.delete;

import com.feyconsuelo.domain.usecase.clouddocument.DeleteDocument;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteCloudDocumentService {

    private final DeleteDocument deleteDocument;

    public ResponseEntity<Void> deleteDocument(final String documentGoogleId) {
        this.deleteDocument.execute(documentGoogleId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
