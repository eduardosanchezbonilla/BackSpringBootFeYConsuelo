package com.feyconsuelo.apirest.service.contactrequest.insert;

import com.feyconsuelo.domain.model.contactrequest.ContactRequest;
import com.feyconsuelo.domain.usecase.contactrequest.InsertContactRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertContactRequestService {

    private final InsertContactRequest insertContactRequest;

    public ResponseEntity<Void> insertContactRequest(final ContactRequest contactRequest) {
        this.insertContactRequest.execute(contactRequest);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

}
