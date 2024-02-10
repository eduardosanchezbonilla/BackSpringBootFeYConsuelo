package com.feyconsuelo.infrastructure.jpa.repository;

import com.feyconsuelo.domain.entity.musician.FindMusiciansRequest;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.infrastructure.jpa.entities.MusicianEntity;
import com.feyconsuelo.infrastructure.jpa.mapper.MusicianEntityMapper;
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
import static org.mockito.Mockito.*;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class MusicianRepositoryImplTest {

    @InjectMocks
    private MusicianRepositoryImpl musicianRepositoryImpl;

    @Mock
    private MusicianJpaRepository musicianJpaRepository;

    @Mock
    private MusicianEntityMapper musicianEntityMapper;

    @Test
    void deleteTest() {

        doNothing().when(this.musicianJpaRepository).deleteById("id");

        this.musicianRepositoryImpl.delete("id");

        verify(this.musicianJpaRepository, times(1)).deleteById("id");
    }

    @Test
    void findTest(@Random final FindMusiciansRequest request,
                  @Random(size = 2, type = MusicianEntity.class) final List<MusicianEntity> musicianEntities,
                  @Random(size = 2, type = Musician.class) final List<Musician> musicians) {

        when(this.musicianJpaRepository.findMusicianRegistries(request)).thenReturn(musicianEntities);
        when(this.musicianEntityMapper.map(musicianEntities)).thenReturn(musicians);

        final List<Musician> result = this.musicianRepositoryImpl.find(request);

        assertThat(result).isEqualTo(musicians);

    }

    @Test
    void getAllTest(@Random(size = 2, type = MusicianEntity.class) final List<MusicianEntity> musicianEntities,
                    @Random(size = 2, type = Musician.class) final List<Musician> musicians) {

        when(this.musicianJpaRepository.findAll()).thenReturn(musicianEntities);
        when(this.musicianEntityMapper.map(musicianEntities)).thenReturn(musicians);

        final List<Musician> result = this.musicianRepositoryImpl.getAll();

        assertThat(result).isEqualTo(musicians);

    }

    @Test
    void getTest(@Random final MusicianEntity musicianEntity, @Random final Musician musician) {

        when(this.musicianJpaRepository.findById("1")).thenReturn(java.util.Optional.ofNullable(musicianEntity));
        when(this.musicianEntityMapper.map(musicianEntity)).thenReturn(musician);

        final Optional<Musician> result = this.musicianRepositoryImpl.get("1");

        assertThat(result.get().getId()).contains(musician.getId());

    }

    @Test
    void saveTest(@Random final Musician musician, @Random final MusicianEntity musicianEntity) {

        when(this.musicianEntityMapper.map(musician)).thenReturn(musicianEntity);
        when(this.musicianJpaRepository.save(musicianEntity)).thenReturn(musicianEntity);
        when(this.musicianEntityMapper.map(musicianEntity)).thenReturn(musician);

        final Musician result = this.musicianRepositoryImpl.save(musician);

        assertThat(result).isEqualTo(musician);

    }
}
