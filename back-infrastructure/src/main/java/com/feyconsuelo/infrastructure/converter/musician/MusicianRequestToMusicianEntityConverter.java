package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.repository.VoiceRepository;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRequestToMusicianEntityConverter {

    private final VoiceRepository voiceRepository;

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;


    public MusicianEntity convert(final MusicianRequest musicianRequest) {
        return MusicianEntity.builder()
                .dni(musicianRequest.getDni())
                .name(musicianRequest.getName())
                .surname(musicianRequest.getSurname())
                .direction(musicianRequest.getDirection())
                .municipality(musicianRequest.getMunicipality())
                .province(musicianRequest.getProvince())
                .email(musicianRequest.getEmail())
                .voice(this.voiceRepository.findVoiceActiveById(musicianRequest.getVoiceId()).orElse(null))
                .image(musicianRequest.getImage())
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public MusicianEntity updateEntity(final MusicianEntity musicianEntity,
                                       final MusicianRequest musicianRequest) {
        musicianEntity.setDni(musicianRequest.getDni());
        musicianEntity.setName(musicianRequest.getName());
        musicianEntity.setSurname(musicianRequest.getSurname());
        musicianEntity.setDirection(musicianRequest.getDirection());
        musicianEntity.setMunicipality(musicianRequest.getMunicipality());
        musicianEntity.setProvince(musicianRequest.getProvince());
        musicianEntity.setEmail(musicianRequest.getEmail());
        musicianEntity.setVoice(this.voiceRepository.findVoiceActiveById(musicianRequest.getVoiceId()).orElse(null));
        musicianEntity.setImage(musicianRequest.getImage());
        musicianEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return musicianEntity;
    }

    public MusicianEntity deleteEntity(final MusicianEntity musicianEntity) {
        musicianEntity.setDeleteDate(LocalDateTime.now());
        musicianEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return musicianEntity;
    }
}
