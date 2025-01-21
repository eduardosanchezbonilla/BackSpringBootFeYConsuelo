package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.infrastructure.converter.voice.VoiceEntityToVoiceResponseConverter;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.entities.voice.VoiceEntity;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MusicianEntityToMusicianRequestConverterTest {

    @Mock
    private VoiceEntityToVoiceResponseConverter voiceEntityToVoiceResponseConverter;

    @InjectMocks
    private MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;

    @Test
    void convert() {
        // setup
        final long id = 1L;
        final String dni = "77337752F";
        final String name = "name";
        final String surname = "surname";
        final String direction = "direction";
        final String municipality = "municipality";
        final String province = "province";
        final String email = "email";
        final Long voiceId = 1L;
        final String image = "image";

        final MusicianEntity musicianEntity = MusicianEntity.builder()
                .id(id)
                .dni(dni)
                .name(name)
                .surname(surname)
                .direction(direction)
                .municipality(municipality)
                .province(province)
                .email(email)
                .voice(
                        VoiceEntity.builder()
                                .id(voiceId)
                                .build()
                )
                .image(image)
                .build();

        // call
        final MusicianResponse result = this.musicianEntityToMusicianResponseConverter.convert(musicianEntity, false);

        // assert
        Assertions.assertEquals(id, result.getId());
        Assertions.assertEquals(dni, result.getDni());
        Assertions.assertEquals(name, result.getName());
        Assertions.assertEquals(surname, result.getSurname());
        Assertions.assertEquals(direction, result.getDirection());
        Assertions.assertEquals(municipality, result.getMunicipality());
        Assertions.assertEquals(province, result.getProvince());
        Assertions.assertEquals(email, result.getEmail());
        // Assertions.assertEquals(voiceId, result.getVoice());  // TODO
        Assertions.assertEquals(image, result.getImage());
    }

}