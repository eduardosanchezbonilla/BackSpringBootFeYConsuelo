package com.feyconsuelo.apirest.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.openapi.model.VoiceRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceRequestDtoToVoiceRequestConverter {
    public VoiceRequest convert(final VoiceRequestDto voiceRequestDto) {
        return VoiceRequest.builder()
                .order(voiceRequestDto.getOrder())
                .name(voiceRequestDto.getName().toUpperCase())
                .image(voiceRequestDto.getImage())
                .build();
    }

}
