package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.apirest.converter.voice.VoiceResponseToVoiceResponseDtoConverter;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianResponseToMusicianResponseDtoConverter {

    private final VoiceResponseToVoiceResponseDtoConverter voiceResponseToVoiceResponseDtoConverter;

    public MusicianResponseDto convert(final MusicianResponse musicianResponse) {
        return MusicianResponseDto.builder()
                .id(musicianResponse.getId())
                .dni(musicianResponse.getDni())
                .name(musicianResponse.getName())
                .surname(musicianResponse.getSurname())
                .direction(musicianResponse.getDirection())
                .municipality(musicianResponse.getMunicipality())
                .province(musicianResponse.getProvince())
                .email(musicianResponse.getEmail())
                .voice(this.voiceResponseToVoiceResponseDtoConverter.convert(musicianResponse.getVoice()))
                .image(musicianResponse.getImage())
                .build();
    }

}
