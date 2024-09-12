package com.feyconsuelo.application.usecase.voice;

import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.domain.usecase.voice.UpdateVoice;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateVoiceImpl implements UpdateVoice {

    private final VoiceService voiceService;

    @Override
    public void execute(final Long voiceId, final VoiceRequest voiceRequest) {
        this.voiceService.update(voiceId, voiceRequest);
    }

}
