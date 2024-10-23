package com.feyconsuelo.infrastructure.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceEntityListToVoiceResponseListConverter {

    private final VoiceEntityToVoiceResponseConverter voiceEntityToVoiceResponseConverter;

    public List<VoiceResponse> convert(final List<VoiceEntity> voiceEntityList) {
        if (CollectionUtils.isEmpty(voiceEntityList)) {
            return List.of();
        }
        return voiceEntityList.stream()
                .map(voice -> this.voiceEntityToVoiceResponseConverter.convert(voice, Boolean.TRUE))
                .toList();
    }
}
