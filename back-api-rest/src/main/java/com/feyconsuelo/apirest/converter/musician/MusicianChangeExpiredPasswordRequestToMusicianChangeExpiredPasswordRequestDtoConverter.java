package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianChangeExpiredPasswordRequest;
import com.feyconsuelo.openapi.model.MusicianChangeExpiredPasswordRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianChangeExpiredPasswordRequestToMusicianChangeExpiredPasswordRequestDtoConverter {

    public MusicianChangeExpiredPasswordRequest convert(final MusicianChangeExpiredPasswordRequestDto musicianChangeExpiredPasswordRequestDto) {
        return MusicianChangeExpiredPasswordRequest.builder()
                .currentPassword(musicianChangeExpiredPasswordRequestDto.getCurrentPassword())
                .newPassword(musicianChangeExpiredPasswordRequestDto.getNewPassword())
                .repeatNewPassword(musicianChangeExpiredPasswordRequestDto.getRepeatNewPassword())
                .build();
    }

}
