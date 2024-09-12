package com.feyconsuelo.apirest.service.musician.insert;

import com.feyconsuelo.apirest.converter.musician.MusicianRequestDtoToMusicianRequestConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.domain.usecase.musician.InsertMusician;
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
public class InsertMusicianService {

    private final InsertMusician insertMusician;

    private final MusicianRequestDtoToMusicianRequestConverter musicianRequestDtoToMusicianRequestConverter;

    private final MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    public ResponseEntity<MusicianResponseDto> postMusician(final MusicianRequestDto musicianRequestDto) {
        return ResponseEntity.status(HttpStatus.CREATED).body(
                this.musicianResponseToMusicianResponseDtoConverter.convert(
                        this.insertMusician.execute(
                                this.musicianRequestDtoToMusicianRequestConverter.convert(musicianRequestDto)
                        )
                )
        );
    }
}
