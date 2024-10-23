package com.feyconsuelo.apirest.service.voice.update;

import com.feyconsuelo.apirest.converter.voice.VoiceRequestDtoToVoiceRequestConverter;
import com.feyconsuelo.domain.usecase.voice.UpdateVoice;
import com.feyconsuelo.openapi.model.VoiceRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateVoiceService {

    private final UpdateVoice updateVoice;

    private final VoiceRequestDtoToVoiceRequestConverter voiceRequestDtoToVoiceRequestConverter;

    public ResponseEntity<Void> updateVoice(final Long voiceId,
                                            final VoiceRequestDto voiceRequestDto) {
        this.updateVoice.execute(
                voiceId,
                this.voiceRequestDtoToVoiceRequestConverter.convert(voiceRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
