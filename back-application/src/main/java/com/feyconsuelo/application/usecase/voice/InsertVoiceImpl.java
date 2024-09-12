package com.feyconsuelo.application.usecase.voice;

import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.domain.usecase.voice.InsertVoice;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertVoiceImpl implements InsertVoice {

    private final VoiceService voiceService;

    @Override
    public void execute(final VoiceRequest voiceRequest) {
        this.voiceService.insert(voiceRequest);
    }

}
