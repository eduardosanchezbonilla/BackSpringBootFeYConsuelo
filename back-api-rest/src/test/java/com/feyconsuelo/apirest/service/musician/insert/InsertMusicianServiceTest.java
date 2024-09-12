package com.feyconsuelo.apirest.service.musician.insert;

import com.feyconsuelo.apirest.converter.musician.MusicianRequestDtoToMusicianRequestConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.InsertMusician;
import com.feyconsuelo.openapi.model.MusicianRequestDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
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
    private MusicianRequestDtoToMusicianRequestConverter musicianRequestDtoToMusicianRequestConverter;

    @Mock
    private MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    @Test
    void postMusicianTest(@Random final MusicianRequestDto musicianRequestDto,
                          @Random final MusicianRequest musicianRequest,
                          @Random final MusicianResponse musicianResponse,
                          @Random final MusicianResponseDto musicianResponseDto) {

        when(this.musicianRequestDtoToMusicianRequestConverter.convert(musicianRequestDto)).thenReturn(musicianRequest);
        when(this.insertMusician.execute(musicianRequest)).thenReturn(musicianResponse);
        when(this.musicianResponseToMusicianResponseDtoConverter.convert(musicianResponse)).thenReturn(musicianResponseDto);

        final ResponseEntity<MusicianResponseDto> response = this.insertMusicianService.postMusician(musicianRequestDto);

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.CREATED);
        assertThat(response.getBody()).isEqualTo(musicianResponseDto);
    }
}
