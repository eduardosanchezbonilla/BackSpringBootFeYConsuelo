package com.feyconsuelo.infrastructure.converter.musician;

import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.CollectionUtils;

import java.util.List;

@ExtendWith(MockitoExtension.class)
class MusicianEntityListToMusicianRequestListConverterTest {

    @Mock
    private MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;
    @InjectMocks
    private MusicianEntityListToMusicianResponseListConverter musicianEntityListToMusicianResponseListConverter;

    @Test
    void convertEmptyList() {
        // call
        Assertions.assertTrue(CollectionUtils.isEmpty(this.musicianEntityListToMusicianResponseListConverter.convert(List.of())));
    }

    @Test
    void convertNotEmptyList() {
        // call
        Assertions.assertFalse(CollectionUtils.isEmpty(this.musicianEntityListToMusicianResponseListConverter.convert(List.of(MusicianEntity.builder().build()))));
    }
}