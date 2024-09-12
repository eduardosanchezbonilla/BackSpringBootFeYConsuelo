package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.infrastructure.converter.voice.VoiceEntityToVoiceResponseConverter;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEntityToMusicianResponseConverter {

    private final VoiceEntityToVoiceResponseConverter voiceEntityToVoiceResponseConverter;

    public MusicianResponse convert(final MusicianEntity musicianEntity) {
        return MusicianResponse.builder()
                .id(musicianEntity.getId())
                .dni(musicianEntity.getDni())
                .name(musicianEntity.getName())
                .surname(musicianEntity.getSurname())
                .direction(musicianEntity.getDirection())
                .municipality(musicianEntity.getMunicipality())
                .province(musicianEntity.getProvince())
                .email(musicianEntity.getEmail())
                .voice(this.voiceEntityToVoiceResponseConverter.convert(musicianEntity.getVoice()))
                .image(musicianEntity.getImage())
                .deleteDate(musicianEntity.getDeleteDate())
                .build();
    }
}
