package com.feyconsuelo.infrastructure.converter.voice;

import com.feyconsuelo.domain.model.voice.VoiceResponse;
import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class VoiceEntityToVoiceResponseConverter {

    public VoiceResponse convert(final VoiceEntity voiceEntity,
                                 final Boolean returnImage
    ) {
        return VoiceResponse.builder()
                .id(voiceEntity.getId())
                .order(voiceEntity.getOrder())
                .name(voiceEntity.getName())
                .image(Boolean.TRUE.equals(returnImage) ? voiceEntity.getImage() : null)
                .deleteDate(voiceEntity.getDeleteDate())
                .build();
    }
}
