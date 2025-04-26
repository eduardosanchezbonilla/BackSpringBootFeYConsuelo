package com.feyconsuelo.apirest.service.contactrequest.markreadunread;

import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.domain.usecase.contactrequest.MarkReadUnreadContactRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class MarkReadUnreadContactRequestService {

    private final MarkReadUnreadContactRequest markReadUnreadContactRequest;

    public ResponseEntity<Void> markReadUnread(final ContactRequest contactRequest) {
        this.markReadUnreadContactRequest.execute(contactRequest);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
