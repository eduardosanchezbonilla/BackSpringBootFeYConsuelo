package com.feyconsuelo.domain.usecase.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactRequest;

public interface MarkReadUnreadContactRequest {

    void execute(final ContactRequest contactRequest);

}
