package com.feyconsuelo.domain.usecase.clouddocument;

import com.feyconsuelo.domain.model.clouddocument.CloudDocumentRequest;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;

import java.util.Optional;

public interface UploadDocument {

    Optional<CloudDocumentResponse> execute(final CloudDocumentRequest contractRequest);

}
