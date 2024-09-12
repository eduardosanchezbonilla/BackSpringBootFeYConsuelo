package com.feyconsuelo.application.usecase.voice;

import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.usecase.voice.DeleteVoice;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteVoiceImpl implements DeleteVoice {

    private final VoiceService voiceService;


    @Override
    public void execute(final Long voiceId) {
        this.voiceService.logicalDelete(voiceId);
    }

}
