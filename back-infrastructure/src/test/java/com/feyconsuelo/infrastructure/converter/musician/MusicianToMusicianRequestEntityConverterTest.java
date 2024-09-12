package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.repository.VoiceRepository;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MusicianToMusicianRequestEntityConverterTest {

    @InjectMocks
    private MusicianRequestToMusicianEntityConverter musicianToMusicianEntityConverter;

    @Mock
    private VoiceRepository voiceRepository;

    @Mock
    private TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Test
    void convert() {
        // setup
        final String dni = "77337752F";
        final String name = "name";
        final String surname = "surname";
        final String direction = "direction";
        final String municipality = "municipality";
        final String province = "province";
        final String email = "email";
        final Long voiceId = 1L;
        final String image = "image";

        final MusicianRequest musicianRequest = MusicianRequest.builder()
                .dni(dni)
                .name(name)
                .surname(surname)
                .direction(direction)
                .municipality(municipality)
                .province(province)
                .email(email)
                .voiceId(voiceId)
                .image(image)
                .build();

        // call
        final MusicianEntity result = this.musicianToMusicianEntityConverter.convert(musicianRequest);

        // assert
        Assertions.assertEquals(dni, result.getDni());
        Assertions.assertEquals(name, result.getName());
        Assertions.assertEquals(surname, result.getSurname());
        Assertions.assertEquals(direction, result.getDirection());
        Assertions.assertEquals(municipality, result.getMunicipality());
        Assertions.assertEquals(province, result.getProvince());
        Assertions.assertEquals(email, result.getEmail());
        Assertions.assertEquals(image, result.getImage());
    }

}