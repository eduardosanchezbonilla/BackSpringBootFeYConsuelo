package com.feyconsuelo.apirest.service.musician.delete;

import com.feyconsuelo.domain.usecase.musician.DeleteMusician;
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

    public ResponseEntity<Void> deleteMusician(final Long musicianId) {
        this.deleteMusician.execute(musicianId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
