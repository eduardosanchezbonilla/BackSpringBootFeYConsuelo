package com.feyconsuelo.domain.usecase.voice;

import com.feyconsuelo.domain.model.voice.VoiceResponse;

import java.util.List;

public interface GetAllVoices {

    List<VoiceResponse> execute();

}
