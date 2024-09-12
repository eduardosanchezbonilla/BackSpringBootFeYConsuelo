package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.CollectionUtils;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class MusicianListToMusicianRequestEntityListConverterTest {

    @Mock
    private MusicianRequestToMusicianEntityConverter musicianToMusicianEntityConverter;
    @InjectMocks
    private MusicianRequestListToMusicianEntityListConverter musicianRequestListToMusicianEntityListConverter;

    @Test
    void convertEmptyList() {
        // call
        Assertions.assertTrue(CollectionUtils.isEmpty(this.musicianRequestListToMusicianEntityListConverter.convert(List.of())));
    }

    @Test
    void convertNotEmptyList() {
        // call
        Assertions.assertFalse(CollectionUtils.isEmpty(this.musicianRequestListToMusicianEntityListConverter.convert(List.of(MusicianRequest.builder().build()))));
    }
}