package com.feyconsuelo.infrastructure.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceRequestToVoiceEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Value("${default-images.voice}")
    private String defaultVoiceImage;

    private String getVoiceImage(final VoiceRequest voiceRequest) {
        if (StringUtils.isEmpty(voiceRequest.getImage())) {
            return voiceRequest.getImage();
        } else {
            if (voiceRequest.getImage().equals(this.defaultVoiceImage)) {
                return null;
            } else {
                return voiceRequest.getImage();
            }
        }
    }

    public VoiceEntity convert(final VoiceRequest voiceRequest) {
        return VoiceEntity.builder()
                .order(voiceRequest.getOrder())
                .name(voiceRequest.getName())
                .image(this.getVoiceImage(voiceRequest))
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public VoiceEntity updateEntity(final VoiceEntity voiceEntity,
                                    final VoiceRequest voiceRequest) {
        voiceEntity.setOrder(voiceRequest.getOrder());
        voiceEntity.setName(voiceRequest.getName());
        voiceEntity.setImage(this.getVoiceImage(voiceRequest));
        voiceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return voiceEntity;
    }

    public VoiceEntity deleteEntity(final VoiceEntity voiceEntity) {
        voiceEntity.setDeleteDate(LocalDateTime.now());
        voiceEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return voiceEntity;
    }
}
