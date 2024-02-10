package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.exception.FeYConsueloNotFoundException;
import io.github.glytching.junit.extension.random.Random;
import io.github.glytching.junit.extension.random.RandomBeansExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class UpdateMusicianImplTest {

    @InjectMocks
    private UpdateMusicianImpl updateMusicianImpl;

    @Mock
    private MusicianRepository musicianRepository;

    @Test
    void executeTest(
            @Random final String musicianId,
            @Random final Musician musician,
            @Random(size = 1, type = Musician.class) final List<Musician> musicians
    ) {

        when(this.musicianRepository.get(musicianId)).thenReturn(Optional.of(musicians.get(0)));

        when(this.musicianRepository.save(Mockito.any())).thenReturn(musician);

        final Musician result = this.updateMusicianImpl.execute(musicianId, musician);

        assertThat(result).isEqualTo(musician);

    }

    @Test
    void executeNotFoundTest(
            @Random final String musicianId,
            @Random final Musician musician,
            @Random(size = 1, type = Musician.class) final List<Musician> musicians
    ) {

        when(this.musicianRepository.get(musicianId))
                .thenReturn(Optional.empty());

        assertThrows(FeYConsueloNotFoundException.class, () -> this.updateMusicianImpl.execute(musicianId, musician));
    }
}
