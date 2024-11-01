package com.feyconsuelo.apirest.service.userpartiturerequest.request;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.domain.usecase.userpartiturerequest.RequestPartiture;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class RequestPartitureService {

    private final RequestPartiture requestPartiture;

    public ResponseEntity<Void> requestPartiture(final UserRequestPartitureRequest userRequestPartitureRequest) {
        this.requestPartiture.execute(userRequestPartitureRequest);
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

}
