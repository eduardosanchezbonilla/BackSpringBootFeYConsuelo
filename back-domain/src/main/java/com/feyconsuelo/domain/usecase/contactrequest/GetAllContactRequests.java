package com.feyconsuelo.domain.usecase.contactrequest;

import com.feyconsuelo.domain.model.contactrequest.ContactResponse;

import java.util.List;

public interface GetAllContactRequests {

    List<ContactResponse> execute(Boolean all);

}
