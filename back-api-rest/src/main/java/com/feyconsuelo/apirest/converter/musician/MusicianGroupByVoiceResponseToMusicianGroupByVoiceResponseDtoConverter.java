package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.apirest.converter.voice.VoiceResponseToVoiceResponseDtoConverter;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceResponse;
import com.feyconsuelo.openapi.model.MusicianGroupByVoiceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianGroupByVoiceResponseToMusicianGroupByVoiceResponseDtoConverter {

    private final VoiceResponseToVoiceResponseDtoConverter voiceResponseToVoiceResponseDtoConverter;
    private final MusicianResponseListToMusicianResponseDtoListConverter musicianResponseListToMusicianResponseDtoListConverter;

    public MusicianGroupByVoiceResponseDto convert(final MusicianGroupByVoiceResponse musicianResponse) {
        return MusicianGroupByVoiceResponseDto.builder()
                .voice(this.voiceResponseToVoiceResponseDtoConverter.convert(musicianResponse.getVoice()))
                .musicians(this.musicianResponseListToMusicianResponseDtoListConverter.convert(musicianResponse.getMusicians()))
                .build();
    }

}
