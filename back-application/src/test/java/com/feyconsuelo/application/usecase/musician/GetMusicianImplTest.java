package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import io.github.glytching.junit.extension.random.Random;
import io.github.glytching.junit.extension.random.RandomBeansExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class GetMusicianImplTest {

    @InjectMocks
    private GetMusicianImpl getMusicianImpl;

    @Mock
    private MusicianService musicianService;

    @Test
    void executeTest(@Random final MusicianResponse musicianResponse) {

        when(this.musicianService.get(1L)).thenReturn(Optional.of(musicianResponse));

        final Optional<MusicianResponse> result = this.getMusicianImpl.execute(1L);

        assertThat(result.get().getId()).isEqualTo(musicianResponse.getId());

    }
}
