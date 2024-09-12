package com.feyconsuelo.apirest.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.openapi.model.VoiceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceResponseToVoiceResponseDtoConverter {

    public VoiceResponseDto convert(final VoiceResponse voiceResponse) {
        return VoiceResponseDto.builder()
                .id(voiceResponse.getId())
                .order(voiceResponse.getOrder())
                .name(voiceResponse.getName())
                .image(voiceResponse.getImage())
                .build();
    }

}
