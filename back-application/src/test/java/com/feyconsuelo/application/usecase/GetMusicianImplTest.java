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

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class GetMusicianImplTest {

    @InjectMocks
    private GetMusicianImpl getMusicianImpl;

    @Mock
    private MusicianRepository musicianRepository;

    @Test
    void executeTest(@Random final Musician musician) {

        when(this.musicianRepository.get("1")).thenReturn(Optional.of(musician));

        final Optional<Musician> result = this.getMusicianImpl.execute("1");

        assertThat(result.get().getId()).contains(musician.getId());

    }
}
