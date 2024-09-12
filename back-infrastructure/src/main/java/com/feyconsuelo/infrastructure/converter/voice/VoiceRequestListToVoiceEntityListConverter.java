package com.feyconsuelo.infrastructure.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceRequestListToVoiceEntityListConverter {

    private final VoiceRequestToVoiceEntityConverter voiceRequestToVoiceEntityConverter;

    public List<VoiceEntity> convert(final List<VoiceRequest> voiceRequestList) {
        if (CollectionUtils.isEmpty(voiceRequestList)) {
            return List.of();
        }
        return voiceRequestList.stream()
                .map(this.voiceRequestToVoiceEntityConverter::convert)
                .toList();
    }
}
