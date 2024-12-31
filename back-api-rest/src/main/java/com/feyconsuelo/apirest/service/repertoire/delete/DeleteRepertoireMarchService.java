package com.feyconsuelo.apirest.service.repertoire.delete;

import com.feyconsuelo.domain.usecase.repertoire.DeleteRepertoireMarch;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteRepertoireMarchService {

    private final DeleteRepertoireMarch deleteRepertoireMarch;

    public ResponseEntity<Void> deleteRepertoireMarch(final Long repertoireMarchId) {
        this.deleteRepertoireMarch.execute(repertoireMarchId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
