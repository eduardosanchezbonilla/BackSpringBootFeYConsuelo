package com.feyconsuelo.infrastructure.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceRequestToVoiceEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public VoiceEntity convert(final VoiceRequest voiceRequest) {
        return VoiceEntity.builder()
                .order(voiceRequest.getOrder())
                .name(voiceRequest.getName())
                .image(voiceRequest.getImage())
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public VoiceEntity updateEntity(final VoiceEntity voiceEntity,
                                    final VoiceRequest voiceRequest) {
        voiceEntity.setOrder(voiceRequest.getOrder());
        voiceEntity.setName(voiceRequest.getName());
        voiceEntity.setImage(voiceRequest.getImage());
        voiceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return voiceEntity;
    }

    public VoiceEntity deleteEntity(final VoiceEntity voiceEntity) {
        voiceEntity.setDeleteDate(LocalDateTime.now());
        voiceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return voiceEntity;
    }
}
