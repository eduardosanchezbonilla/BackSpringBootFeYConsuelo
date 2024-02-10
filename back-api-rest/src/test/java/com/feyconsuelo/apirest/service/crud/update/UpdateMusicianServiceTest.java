package com.feyconsuelo.apirest.service.crud.update;

import com.feyconsuelo.apirest.mapper.MusicianMapper;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.UpdateMusician;
import com.feyconsuelo.openapi.model.MusicianDTO;
import com.feyconsuelo.openapi.model.MusicianResponseDTO;
import io.github.glytching.junit.extension.random.Random;
import io.github.glytching.junit.extension.random.RandomBeansExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class UpdateMusicianServiceTest {

    @InjectMocks
    private UpdateMusicianService updateMusicianService;

    @Mock
    private UpdateMusician updateMusician;

    @Mock
    private MusicianMapper musicianMapper;

    @Test
    void updateMusicianTest(@Random final String musicianId,
                            @Random final MusicianDTO musicianDTO,
                            @Random final Musician musician,
                            @Random final MusicianResponseDTO musicianResponseDTO) {

        when(this.musicianMapper.map(musicianDTO)).thenReturn(musician);
        when(this.updateMusician.execute(musicianId, musician)).thenReturn(musician);
        when(this.musicianMapper.map(musician)).thenReturn(musicianResponseDTO);

        final MusicianResponseDTO response = this.updateMusicianService.updateMusician(musicianId, musicianDTO).getBody();

        assertThat(response).isEqualTo(musicianResponseDTO);

    }

}
