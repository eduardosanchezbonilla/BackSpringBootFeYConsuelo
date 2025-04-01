package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.repository.VoiceRepository;
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
public class MusicianRequestToMusicianEntityConverter {

    private final VoiceRepository voiceRepository;

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Value("${default-images.musician}")
    private String defaultMusicianImage;

    @Value("${default-images.user}")
    private String defaultUserImage;

    private String getMusicianImage(final String image) {
        if (StringUtils.isEmpty(image)) {
            return null;
        } else {
            if (image.equals(this.defaultMusicianImage) || image.equals(this.defaultUserImage)) {
                return null;
            } else {
                return image;
            }
        }
    }


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
                .image(this.getMusicianImage(musicianRequest.getImage()))
                .imageThumbnail(this.getMusicianImage(musicianRequest.getImageThumbnail()))
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .birthDate(musicianRequest.getBirthDate())
                .registrationDate(musicianRequest.getRegistrationDate())
                .unregistrationDate(musicianRequest.getUnregistrationDate())
                .unregistred(musicianRequest.getUnregistred() == null ? Boolean.FALSE : musicianRequest.getUnregistred())
                .dateLastNotificationNonAssistsStreakRehearsals(musicianRequest.getDateLastNotificationNonAssistsStreakRehearsals() == null ? LocalDateTime.now() : musicianRequest.getDateLastNotificationNonAssistsStreakRehearsals())
                .inventoryObservations(musicianRequest.getInventoryObservations())
                .phoneNumber(musicianRequest.getPhoneNumber())
                .observations(musicianRequest.getObservations())
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
        musicianEntity.setImage(this.getMusicianImage(musicianRequest.getImage()));
        musicianEntity.setImageThumbnail(this.getMusicianImage(musicianRequest.getImageThumbnail()));
        musicianEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());
        musicianEntity.setBirthDate(musicianRequest.getBirthDate());
        musicianEntity.setRegistrationDate(musicianRequest.getRegistrationDate());
        musicianEntity.setInventoryObservations(musicianRequest.getInventoryObservations());
        musicianEntity.setPhoneNumber(musicianRequest.getPhoneNumber());
        musicianEntity.setUnregistrationDate(musicianRequest.getUnregistrationDate());
        musicianEntity.setUnregistred(musicianRequest.getUnregistred() == null ? Boolean.FALSE : musicianRequest.getUnregistred());
        musicianEntity.setObservations(musicianRequest.getObservations());

        return musicianEntity;
    }

    public MusicianEntity deleteEntity(final MusicianEntity musicianEntity) {
        musicianEntity.setDeleteDate(LocalDateTime.now());
        musicianEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return musicianEntity;
    }
}
