package com.feyconsuelo.application.usecase.voice;

import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.domain.usecase.voice.GetVoice;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetVoiceImpl implements GetVoice {

    private final VoiceService voiceService;

    @Override
    public Optional<VoiceResponse> execute(final Long voiceId) {
        return this.voiceService.get(voiceId);
    }
}
