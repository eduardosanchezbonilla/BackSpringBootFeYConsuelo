package com.feyconsuelo.apirest.service.voice;

import com.feyconsuelo.apirest.service.voice.delete.DeleteVoiceService;
import com.feyconsuelo.apirest.service.voice.insert.InsertVoiceService;
import com.feyconsuelo.apirest.service.voice.query.GetVoiceService;
import com.feyconsuelo.apirest.service.voice.update.UpdateVoiceService;
import com.feyconsuelo.openapi.api.VoiceControllerApiDelegate;
import com.feyconsuelo.openapi.model.VoiceRequestDto;
import com.feyconsuelo.openapi.model.VoiceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class VoiceApiService implements VoiceControllerApiDelegate {

    private final DeleteVoiceService deleteVoiceService;
    private final InsertVoiceService insertVoiceService;
    private final UpdateVoiceService updateVoiceService;
    private final GetVoiceService getVoiceService;

    @Override
    public ResponseEntity<Void> deleteVoice(final Long voiceId) {
        return this.deleteVoiceService.deleteVoice(voiceId);
    }

    @Override
    public ResponseEntity<Void> postVoice(final VoiceRequestDto voiceRequestDto) {
        return this.insertVoiceService.postVoice(voiceRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateVoice(final Long voiceId,
                                            final VoiceRequestDto voiceRequestDto) {
        return this.updateVoiceService.updateVoice(voiceId, voiceRequestDto);
    }


    @Override
    public ResponseEntity<List<VoiceResponseDto>> getAllVoices() {
        return this.getVoiceService.getAllVoices();
    }

    @Override
    public ResponseEntity<VoiceResponseDto> getVoice(final Long voiceId) {
        return this.getVoiceService.getVoice(voiceId);
    }

}
