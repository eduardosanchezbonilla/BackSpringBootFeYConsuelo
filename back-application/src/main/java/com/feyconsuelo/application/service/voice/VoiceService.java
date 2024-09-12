package com.feyconsuelo.application.service.voice;

import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.domain.model.voice.VoiceResponse;

import java.util.List;
import java.util.Optional;

public interface VoiceService {

    void delete(Long voiceId);

    void logicalDelete(Long voiceId);

    List<VoiceResponse> getAll();

    Optional<VoiceResponse> get(Long voiceId);

    void insert(VoiceRequest voiceRequest);

    void update(Long voiceId, VoiceRequest voiceRequest);

}
