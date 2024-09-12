package com.feyconsuelo.apirest.service.voice.delete;

import com.feyconsuelo.domain.usecase.voice.DeleteVoice;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteVoiceService {

    private final DeleteVoice deleteVoice;

    public ResponseEntity<Void> deleteVoice(final Long voiceId) {
        this.deleteVoice.execute(voiceId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
