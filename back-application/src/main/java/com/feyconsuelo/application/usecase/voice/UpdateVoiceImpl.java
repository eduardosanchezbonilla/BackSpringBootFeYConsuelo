package com.feyconsuelo.application.usecase.voice;

import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.voice.VoiceRequest;
import com.feyconsuelo.domain.usecase.voice.UpdateVoice;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateVoiceImpl implements UpdateVoice {

    private final VoiceService voiceService;
    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.voice}")
    private String defaultVoiceImage;

    @Override
    public void execute(final Long voiceId, final VoiceRequest voiceRequest) {
        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(voiceRequest.getImage()) && !voiceRequest.getImage().equals(this.defaultVoiceImage)) {
            voiceRequest.setImage(this.resizeImageService.resizeImage(voiceRequest.getImage()));
        }

        this.voiceService.update(voiceId, voiceRequest);
    }

}
