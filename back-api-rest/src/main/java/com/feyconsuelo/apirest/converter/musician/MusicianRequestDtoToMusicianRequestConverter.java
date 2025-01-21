package com.feyconsuelo.apirest.converter.musician;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.openapi.model.MusicianRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianRequestDtoToMusicianRequestConverter {

    private final DateService dateService;
    @Value("${default-images.musician}")
    private String defaultVoiceMusician;

    private String getMusicianImage(final String image) {
        if (StringUtils.isEmpty(image)) {
            return null;
        } else {
            if (image.equals(this.defaultVoiceMusician)) {
                return null;
            } else {
                return image;
            }
        }
    }

    public MusicianRequest convert(final MusicianRequestDto musicianRequestDto) {
        return MusicianRequest.builder()
                .dni(musicianRequestDto.getDni().toUpperCase())
                .name(musicianRequestDto.getName().toUpperCase())
                .surname(musicianRequestDto.getSurname().toUpperCase())
                .direction(musicianRequestDto.getDirection().toUpperCase())
                .municipality(musicianRequestDto.getMunicipality().toUpperCase())
                .province(musicianRequestDto.getProvince().toUpperCase())
                .email(musicianRequestDto.getEmail())
                .voiceId(musicianRequestDto.getVoiceId())
                .image(this.getMusicianImage(musicianRequestDto.getImage()))
                .birthDate(this.dateService.stringToDate(musicianRequestDto.getBirthDate(), DateTimeFormatter.ISO_DATE_TIME))
                .registrationDate(this.dateService.stringToDate(musicianRequestDto.getRegistrationDate(), DateTimeFormatter.ISO_DATE_TIME))
                .inventoryObservations(musicianRequestDto.getInventoryObservations())
                .phoneNumber(musicianRequestDto.getPhoneNumber())
                .build();
    }

}
