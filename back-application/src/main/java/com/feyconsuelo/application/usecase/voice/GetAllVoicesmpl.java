package com.feyconsuelo.application.usecase.voice;

import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.domain.usecase.voice.GetAllVoices;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllVoicesmpl implements GetAllVoices {

    private final VoiceService voiceService;

    @Override
    public List<VoiceResponse> execute() {
        return this.voiceService.getAll();
    }
}
