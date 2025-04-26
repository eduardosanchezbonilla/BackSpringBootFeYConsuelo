package com.feyconsuelo.domain.usecase.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactRequest;

public interface InsertContactRequest {

    void execute(final ContactRequest contactRequest);

}
