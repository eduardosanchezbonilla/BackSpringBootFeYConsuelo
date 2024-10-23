package com.feyconsuelo.application.usecase.voice;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.voice.VoiceService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.voice.DeleteVoice;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class DeleteVoiceImpl implements DeleteVoice {

    private final VoiceService voiceService;
    private final MusicianService musicianService;


    @Override
    public void execute(final Long voiceId) {
        // solo permitiremos borrar voces que no esten asociadas a ningun musico
        final List<MusicianResponse> musicians = this.musicianService.getByVoice(voiceId);
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musicians))) {
            throw new BadRequestException("No se pueden eliminar voces asociadas a musicos");
        }
        this.voiceService.logicalDelete(voiceId);
    }

}
