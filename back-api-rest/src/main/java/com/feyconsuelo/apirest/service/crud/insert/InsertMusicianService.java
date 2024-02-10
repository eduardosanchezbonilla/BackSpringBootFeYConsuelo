package com.feyconsuelo.apirest.service.crud.insert;

import com.feyconsuelo.apirest.mapper.MusicianMapper;
import com.feyconsuelo.domain.usecase.InsertMusician;
import com.feyconsuelo.openapi.model.MusicianDTO;
import com.feyconsuelo.openapi.model.MusicianResponseDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertMusicianService {

    private final InsertMusician insertMusician;

    private final MusicianMapper musicianMapper;

    public ResponseEntity<MusicianResponseDTO> postMusician(final MusicianDTO musicianDTO) {
        return ResponseEntity.status(HttpStatus.CREATED).body(
                this.musicianMapper.map(
                        this.insertMusician.execute(
                                this.musicianMapper.map(musicianDTO)
                        )
                )
        );
    }
}
