package com.feyconsuelo.application.service.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.domain.model.contactrequest.ContactResponse;

import java.util.List;

public interface ContactRequestService {

    void insert(ContactRequest contactRequest);

    List<ContactResponse> getAllContactRequest();

    void markReadUnread(ContactRequest contactRequest);

}
