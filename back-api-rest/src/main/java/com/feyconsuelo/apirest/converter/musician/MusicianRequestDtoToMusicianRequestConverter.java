package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.openapi.model.MusicianRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRequestDtoToMusicianRequestConverter {
    public MusicianRequest convert(final MusicianRequestDto musicianRequestDto) {
        return MusicianRequest.builder()
                .dni(musicianRequestDto.getDni().toUpperCase())
                .name(musicianRequestDto.getName().toUpperCase())
                .surname(musicianRequestDto.getSurname().toUpperCase())
                .direction(musicianRequestDto.getDirection().toUpperCase())
                .municipality(musicianRequestDto.getMunicipality().toUpperCase())
                .province(musicianRequestDto.getProvince().toUpperCase())
                .email(musicianRequestDto.getEmail())
                .voiceId(musicianRequestDto.getVoiceId())
                .image(musicianRequestDto.getImage())
                .build();
    }

}
