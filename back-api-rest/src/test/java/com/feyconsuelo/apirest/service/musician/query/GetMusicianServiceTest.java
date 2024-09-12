package com.feyconsuelo.apirest.service.musician.query;

import com.feyconsuelo.apirest.converter.musician.MusicianResponseListToMusicianResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.GetAllMusicians;
import com.feyconsuelo.domain.usecase.musician.GetMusician;
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

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class GetMusicianServiceTest {

    @InjectMocks
    private GetMusicianService getMusicianService;

    @Mock
    private GetAllMusicians getAllMusicians;

    @Mock
    private GetMusician getMusician;

    @Mock
    private MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    @Mock
    private MusicianResponseListToMusicianResponseDtoListConverter musicianResponseListToMusicianResponseDtoListConverter;

    @Test
    void getAllMusiciansTest(@Random(size = 2, type = MusicianResponseDto.class) final List<MusicianResponseDto> musicianResponseDtoList,
                             @Random(size = 2, type = MusicianResponse.class) final List<MusicianResponse> musicianResponseList) {

        when(this.getAllMusicians.execute()).thenReturn(musicianResponseList);
        when(this.musicianResponseListToMusicianResponseDtoListConverter.convert(musicianResponseList)).thenReturn(musicianResponseDtoList);

        final ResponseEntity<List<MusicianResponseDto>> result = this.getMusicianService.getAllMusicians();

        assertThat(result.getBody()).isEqualTo(musicianResponseDtoList);
        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.OK);

    }

    @Test
    void getAllMusiciansNoContentTest() {

        when(this.getAllMusicians.execute()).thenReturn(List.of());

        final ResponseEntity<List<MusicianResponseDto>> result = this.getMusicianService.getAllMusicians();

        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.NO_CONTENT);

    }

    @Test
    void getMusicianTest(@Random final MusicianResponse musicianResponse,
                         @Random final MusicianResponseDto musicianResponseDto) {

        when(this.getMusician.execute(1L)).thenReturn(Optional.of(musicianResponse));
        when(this.musicianResponseToMusicianResponseDtoConverter.convert(musicianResponse)).thenReturn(musicianResponseDto);

        final ResponseEntity<MusicianResponseDto> result = this.getMusicianService.getMusician(1L);

        assertThat(result.getBody()).isEqualTo(musicianResponseDto);
        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.OK);

    }

}
