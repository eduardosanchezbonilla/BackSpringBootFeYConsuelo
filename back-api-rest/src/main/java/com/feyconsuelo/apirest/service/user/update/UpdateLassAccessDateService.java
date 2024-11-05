package com.feyconsuelo.apirest.service.user.update;

import com.feyconsuelo.domain.usecase.user.UpdateUserLastAccessDate;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateLassAccessDateService {

    private final UpdateUserLastAccessDate updateUserLastAccessDate;

    public ResponseEntity<Void> updateUserLastAccessDate(final String username) {
        this.updateUserLastAccessDate.execute(username.toLowerCase());
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
