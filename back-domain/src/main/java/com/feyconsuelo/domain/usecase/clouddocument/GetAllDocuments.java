package com.feyconsuelo.domain.usecase.clouddocument;

import com.feyconsuelo.domain.model.clouddocument.CloudDocumentRequest;
import com.feyconsuelo.domain.model.clouddocument.CloudDocumentResponse;

import java.util.List;

public interface GetAllDocuments {

    List<CloudDocumentResponse> execute(final CloudDocumentRequest cloudDocumentRequest);

}
