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

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class GetAllMusiciansImplTest {

    @InjectMocks
    private GetAllMusiciansImpl getAllMusiciansImpl;

    @Mock
    private MusicianService musicianService;

    @Test
    void executeTest(@Random(size = 2, type = MusicianResponse.class) final List<MusicianResponse> musicianResponseList) {

        when(this.musicianService.getAll(Boolean.FALSE)).thenReturn(musicianResponseList);

        final List<MusicianResponse> result = this.getAllMusiciansImpl.execute(Boolean.FALSE);

        assertThat(result).isEqualTo(musicianResponseList);

    }
}
