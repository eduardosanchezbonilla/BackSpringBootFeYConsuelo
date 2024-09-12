package com.feyconsuelo.domain.usecase.voice;

import com.feyconsuelo.domain.model.voice.VoiceRequest;

public interface InsertVoice {

    void execute(VoiceRequest voiceRequest);

}
