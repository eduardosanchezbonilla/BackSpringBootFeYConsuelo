package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.apirest.converter.voice.VoiceResponseToVoiceResponseDtoConverter;
import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianResponseToMusicianResponseDtoConverter {

    private final VoiceResponseToVoiceResponseDtoConverter voiceResponseToVoiceResponseDtoConverter;
    private final DateService dateService;

    public MusicianResponseDto convert(final MusicianResponse musicianResponse) {
        return MusicianResponseDto.builder()
                .id(musicianResponse.getId())
                .dni(musicianResponse.getDni())
                .name(musicianResponse.getName())
                .surname(musicianResponse.getSurname())
                .direction(musicianResponse.getDirection())
                .municipality(musicianResponse.getMunicipality())
                .province(musicianResponse.getProvince())
                .email(musicianResponse.getEmail())
                .voice(this.voiceResponseToVoiceResponseDtoConverter.convert(musicianResponse.getVoice()))
                .image(musicianResponse.getImage())
                .birthDate(this.dateService.dateToString(musicianResponse.getBirthDate(), DateTimeFormatter.ISO_DATE_TIME))
                .registrationDate(this.dateService.dateToString(musicianResponse.getRegistrationDate(), DateTimeFormatter.ISO_DATE_TIME))
                .unregistrationDate(this.dateService.dateToString(musicianResponse.getUnregistrationDate(), DateTimeFormatter.ISO_DATE_TIME))
                .unregistred(musicianResponse.getUnregistred() != null && musicianResponse.getUnregistred())
                .dateLastNotificationNonAssistsStreakRehearsals(this.dateService.dateToString(musicianResponse.getDateLastNotificationNonAssistsStreakRehearsals(), DateTimeFormatter.ISO_DATE_TIME))
                .inventoryObservations(musicianResponse.getInventoryObservations())
                .idLastRehearsal(musicianResponse.getIdLastRehearsal())
                .assistLastRehearsal(musicianResponse.getAssistLastRehearsal())
                .dateLastRehearsal(musicianResponse.getDateLastRehearsal())
                .phoneNumber(musicianResponse.getPhoneNumber())
                .observations(musicianResponse.getObservations())
                .build();
    }

}
