package com.feyconsuelo.apirest.service.musician.changeexpiredpassword;

import com.feyconsuelo.apirest.converter.musician.MusicianChangeExpiredPasswordRequestToMusicianChangeExpiredPasswordRequestDtoConverter;
import com.feyconsuelo.domain.usecase.musician.ChangeExpiredPasswordMusician;
import com.feyconsuelo.openapi.model.MusicianChangeExpiredPasswordRequestDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianChangeExpiredPasswordService {

    private final ChangeExpiredPasswordMusician changeExpiredPasswordMusician;

    private final MusicianChangeExpiredPasswordRequestToMusicianChangeExpiredPasswordRequestDtoConverter musicianChangeExpiredPasswordRequestToMusicianChangeExpiredPasswordRequestDtoConverter;

    public ResponseEntity<MusicianResponseDto> changeExpiredPassword(final String dni,
                                                                     final MusicianChangeExpiredPasswordRequestDto musicianChangeExpiredPasswordRequestDto) {
        this.changeExpiredPasswordMusician.execute(
                dni,
                this.musicianChangeExpiredPasswordRequestToMusicianChangeExpiredPasswordRequestDtoConverter.convert(musicianChangeExpiredPasswordRequestDto)
        );

        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
