package com.feyconsuelo.infrastructure.service.musician;

import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityListToMusicianResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musician.MusicianEntityToMusicianResponseConverter;
import com.feyconsuelo.infrastructure.converter.musician.MusicianRequestToMusicianEntityConverter;
import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.repository.MusicianRepository;
import io.github.glytching.junit.extension.random.Random;
import io.github.glytching.junit.extension.random.RandomBeansExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class MusicianRequestServiceImplTest {

    @InjectMocks
    private MusicianServiceImpl musicianServiceImpl;
    @Mock
    private MusicianRepository musicianRepository;
    @Mock
    private MusicianRequestToMusicianEntityConverter musicianToMusicianEntityConverter;
    @Mock
    private MusicianEntityListToMusicianResponseListConverter musicianEntityListToMusicianResponseListConverter;
    @Mock
    private MusicianEntityToMusicianResponseConverter musicianEntityToMusicianResponseConverter;

    @Test
    void deleteTest() {

        doNothing().when(this.musicianRepository).deleteById(1L);

        this.musicianServiceImpl.delete(1L);

        verify(this.musicianRepository, times(1)).deleteById(1L);
    }

    @Test
    void getAllTest(@Random(size = 2, type = MusicianEntity.class) final List<MusicianEntity> musicianEntities,
                    @Random(size = 2, type = MusicianResponse.class) final List<MusicianResponse> musicianResponses) {

        when(this.musicianRepository.findAllActives(Boolean.FALSE)).thenReturn(musicianEntities);
        when(this.musicianEntityListToMusicianResponseListConverter.convert(musicianEntities)).thenReturn(musicianResponses);

        final List<MusicianResponse> result = this.musicianServiceImpl.getAll(Boolean.FALSE);

        assertThat(result).isEqualTo(musicianResponses);

    }

    @Test
    void getTest(@Random final MusicianEntity musicianEntity, @Random final MusicianResponse musicianResponses) {

        when(this.musicianRepository.findMusicianActiveById(1L)).thenReturn(java.util.Optional.ofNullable(musicianEntity));
        when(this.musicianEntityToMusicianResponseConverter.convert(musicianEntity, true)).thenReturn(musicianResponses);

        final Optional<MusicianResponse> result = this.musicianServiceImpl.get(1L, true);

        assertThat(result.get().getId()).isEqualTo(musicianResponses.getId());

    }

    @Test
    void saveTest(@Random final MusicianRequest musicianRequest,
                  @Random final MusicianResponse musicianResponse,
                  @Random final MusicianEntity musicianEntity) {

        when(this.musicianToMusicianEntityConverter.convert(musicianRequest)).thenReturn(musicianEntity);
        when(this.musicianRepository.save(musicianEntity)).thenReturn(musicianEntity);
        when(this.musicianEntityToMusicianResponseConverter.convert(musicianEntity, true)).thenReturn(musicianResponse);

        final MusicianResponse result = this.musicianServiceImpl.insert(musicianRequest);

        assertThat(result).isEqualTo(musicianResponse);

    }
}
