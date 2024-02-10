package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
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
class InsertMusicianImplTest {

    @InjectMocks
    private InsertMusicianImpl insertMusicianImpl;

    @Mock
    private MusicianRepository musicianRepository;

    @Test
    void executeTest(@Random final Musician musician) {

        when(this.musicianRepository.save(musician)).thenReturn(musician);

        final Musician result = this.insertMusicianImpl.execute(musician);

        assertThat(result).isEqualTo(musician);

    }
}
