package com.feyconsuelo.domain.usecase.voice;

import com.feyconsuelo.domain.model.voice.VoiceResponse;

import java.util.Optional;

public interface GetVoice {

    Optional<VoiceResponse> execute(Long voiceId);

}
