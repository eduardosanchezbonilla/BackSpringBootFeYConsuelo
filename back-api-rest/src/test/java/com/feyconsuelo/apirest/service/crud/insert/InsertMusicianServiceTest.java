package com.feyconsuelo.apirest.service.crud.insert;

import com.feyconsuelo.apirest.mapper.MusicianMapper;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.usecase.InsertMusician;
import com.feyconsuelo.openapi.model.MusicianDTO;
import com.feyconsuelo.openapi.model.MusicianResponseDTO;
import io.github.glytching.junit.extension.random.Random;
import io.github.glytching.junit.extension.random.RandomBeansExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class InsertMusicianServiceTest {

    @InjectMocks
    private InsertMusicianService insertMusicianService;

    @Mock
    private InsertMusician insertMusician;

    @Mock
    private MusicianMapper musicianMapper;

    @Test
    void postMusicianTest(@Random final MusicianDTO musicianDTO,
                          @Random final Musician musician,
                          @Random final MusicianResponseDTO musicianResponseDTO) {

        when(this.musicianMapper.map(musicianDTO)).thenReturn(musician);
        when(this.insertMusician.execute(musician)).thenReturn(musician);
        when(this.musicianMapper.map(musician)).thenReturn(musicianResponseDTO);

        final ResponseEntity<MusicianResponseDTO> response = this.insertMusicianService.postMusician(musicianDTO);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        assertThat(response.getBody()).isEqualTo(musicianResponseDTO);
    }
}
