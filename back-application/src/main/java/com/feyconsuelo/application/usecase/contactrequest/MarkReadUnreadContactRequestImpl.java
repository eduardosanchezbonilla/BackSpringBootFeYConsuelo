package com.feyconsuelo.application.usecase.contactrequest;

import com.feyconsuelo.application.service.contactrequest.ContactRequestService;
import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.domain.usecase.contactrequest.MarkReadUnreadContactRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class MarkReadUnreadContactRequestImpl implements MarkReadUnreadContactRequest {

    private final ContactRequestService contactRequestService;

    @Override
    public void execute(final ContactRequest contactRequest) {
        // registramos la soliccitud
        this.contactRequestService.markReadUnread(contactRequest);
    }
}