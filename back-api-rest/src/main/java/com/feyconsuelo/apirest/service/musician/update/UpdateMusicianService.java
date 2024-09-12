package com.feyconsuelo.apirest.service.musician.update;

import com.feyconsuelo.apirest.converter.musician.MusicianRequestDtoToMusicianRequestConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.domain.usecase.musician.UpdateMusician;
import com.feyconsuelo.openapi.model.MusicianRequestDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
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

    private final MusicianRequestDtoToMusicianRequestConverter musicianRequestDtoToMusicianRequestConverter;

    private final MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    public ResponseEntity<MusicianResponseDto> updateMusician(final Long musicianId,
                                                              final MusicianRequestDto musicianRequestDto) {
        return ResponseEntity.status(HttpStatus.OK).body(
                this.musicianResponseToMusicianResponseDtoConverter.convert(
                        this.updateMusician.execute(
                                musicianId,
                                this.musicianRequestDtoToMusicianRequestConverter.convert(musicianRequestDto)
                        )
                )
        );
    }
}
