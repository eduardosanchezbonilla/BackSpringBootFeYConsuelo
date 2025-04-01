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

    public MusicianResponse convert(final MusicianEntity musicianEntity,
                                    final Boolean isThumbnail) {
        return MusicianResponse.builder()
                .id(musicianEntity.getId())
                .dni(musicianEntity.getDni())
                .name(musicianEntity.getName())
                .surname(musicianEntity.getSurname())
                .direction(musicianEntity.getDirection())
                .municipality(musicianEntity.getMunicipality())
                .province(musicianEntity.getProvince())
                .email(musicianEntity.getEmail())
                .voice(this.voiceEntityToVoiceResponseConverter.convert(musicianEntity.getVoice(), Boolean.FALSE))
                .image(Boolean.TRUE.equals(isThumbnail) ? musicianEntity.getImageThumbnail() : musicianEntity.getImage())
                .deleteDate(musicianEntity.getDeleteDate())
                .birthDate(musicianEntity.getBirthDate())
                .registrationDate(musicianEntity.getRegistrationDate())
                .unregistrationDate(musicianEntity.getUnregistrationDate())
                .unregistred(musicianEntity.getUnregistred() == null ? Boolean.FALSE : musicianEntity.getUnregistred())
                .dateLastNotificationNonAssistsStreakRehearsals(musicianEntity.getDateLastNotificationNonAssistsStreakRehearsals())
                .inventoryObservations(musicianEntity.getInventoryObservations())
                .phoneNumber(musicianEntity.getPhoneNumber())
                .observations(musicianEntity.getObservations())
                .build();
    }
}
