package com.feyconsuelo.apirest.service.userpartiturerequest.markreadunread;

import com.feyconsuelo.domain.model.userpartiturerequest.UserRequestPartitureRequest;
import com.feyconsuelo.domain.usecase.userpartiturerequest.MarkReadUnreadRequestPartiture;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class MarkReadUnreadRequestPartitureService {

    private final MarkReadUnreadRequestPartiture markReadUnreadRequestPartiture;

    public ResponseEntity<Void> markReadUnread(final UserRequestPartitureRequest userRequestPartitureRequest) {
        this.markReadUnreadRequestPartiture.execute(userRequestPartitureRequest);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
