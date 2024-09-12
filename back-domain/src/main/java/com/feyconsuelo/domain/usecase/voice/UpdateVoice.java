package com.feyconsuelo.domain.usecase.voice;

import com.feyconsuelo.domain.model.voice.VoiceRequest;

public interface UpdateVoice {

    void execute(Long voiceId, VoiceRequest voiceRequest);

}
