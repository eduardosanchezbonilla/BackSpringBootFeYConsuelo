package com.feyconsuelo.apirest.service.crud.delete;

import com.feyconsuelo.domain.usecase.DeleteMusician;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteMusicianService {

    private final DeleteMusician deleteMusician;

    public ResponseEntity<Void> deleteMusician(final String musicianId) {
        this.deleteMusician.execute(musicianId);
        return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    }

}
