package com.feyconsuelo.apirest.service.musician.update;

import com.feyconsuelo.apirest.converter.musician.MusicianRequestDtoToMusicianRequestConverter;
import com.feyconsuelo.apirest.converter.musician.MusicianResponseToMusicianResponseDtoConverter;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musician.UpdateMusician;
import com.feyconsuelo.openapi.model.MusicianRequestDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
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
    private MusicianRequestDtoToMusicianRequestConverter musicianRequestDtoToMusicianRequestConverter;

    @Mock
    private MusicianResponseToMusicianResponseDtoConverter musicianResponseToMusicianResponseDtoConverter;

    @Test
    void updateMusicianTest(@Random final Long musicianId,
                            @Random final MusicianRequestDto musicianRequestDto,
                            @Random final MusicianRequest musicianRequest,
                            @Random final MusicianResponse musicianResponse,
                            @Random final MusicianResponseDto musicianResponseDto) {

        when(this.musicianRequestDtoToMusicianRequestConverter.convert(musicianRequestDto)).thenReturn(musicianRequest);
        when(this.updateMusician.execute(musicianId, musicianRequest)).thenReturn(musicianResponse);
        when(this.musicianResponseToMusicianResponseDtoConverter.convert(musicianResponse)).thenReturn(musicianResponseDto);

        final MusicianResponseDto response = this.updateMusicianService.updateMusician(musicianId, musicianRequestDto).getBody();

        assertThat(response).isEqualTo(musicianResponseDto);

    }

}

