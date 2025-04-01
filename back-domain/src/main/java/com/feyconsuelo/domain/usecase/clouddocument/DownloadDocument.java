package com.feyconsuelo.domain.usecase.clouddocument;

import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;

import java.util.Optional;

public interface DownloadDocument {

    Optional<CloudDocumentResponse> execute(final String documentGoogleId);

}
