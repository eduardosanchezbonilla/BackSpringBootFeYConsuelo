package com.feyconsuelo.apirest.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.openapi.model.VoiceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceResponseListToVoiceResponseDtoListConverter {

    private final VoiceResponseToVoiceResponseDtoConverter voiceResponseToVoiceResponseDtoConverter;

    public List<VoiceResponseDto> convert(final List<VoiceResponse> voiceResponseList) {
        if (CollectionUtils.isEmpty(voiceResponseList)) {
            return List.of();
        }
        return voiceResponseList.stream()
                .map(voiceResponseToVoiceResponseDtoConverter::convert)
                .toList();
    }

}
