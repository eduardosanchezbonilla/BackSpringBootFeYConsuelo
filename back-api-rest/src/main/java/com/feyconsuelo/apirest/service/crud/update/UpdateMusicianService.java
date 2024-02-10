package com.feyconsuelo.apirest.service.crud.update;

import com.feyconsuelo.apirest.mapper.MusicianMapper;
import com.feyconsuelo.domain.usecase.UpdateMusician;
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
public class UpdateMusicianService {

    private final UpdateMusician updateMusician;

    private final MusicianMapper musicianMapper;

    public ResponseEntity<MusicianResponseDTO> updateMusician(final String musicianId,
                                                              final MusicianDTO musicianDTO) {
        return ResponseEntity.status(HttpStatus.OK).body(
                this.musicianMapper.map(
                        this.updateMusician.execute(
                                musicianId,
                                this.musicianMapper.map(musicianDTO)
                        )
                )
        );
    }
}
