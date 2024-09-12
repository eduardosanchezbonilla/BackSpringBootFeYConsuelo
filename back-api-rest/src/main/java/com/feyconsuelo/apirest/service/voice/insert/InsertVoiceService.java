package com.feyconsuelo.apirest.service.voice.insert;

import com.feyconsuelo.apirest.converter.voice.VoiceRequestDtoToVoiceRequestConverter;
import com.feyconsuelo.domain.usecase.voice.InsertVoice;
import com.feyconsuelo.openapi.model.VoiceRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertVoiceService {

    private final InsertVoice insertVoice;

    private final VoiceRequestDtoToVoiceRequestConverter voiceRequestDtoToVoiceRequestConverter;

    public ResponseEntity<Void> postVoice(final VoiceRequestDto voiceRequestDto) {
        this.insertVoice.execute(
                this.voiceRequestDtoToVoiceRequestConverter.convert(voiceRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
