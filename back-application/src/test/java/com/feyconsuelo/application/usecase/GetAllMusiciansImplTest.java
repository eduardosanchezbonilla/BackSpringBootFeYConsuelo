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

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class GetAllMusiciansImplTest {

    @InjectMocks
    private GetAllMusiciansImpl getAllMusiciansImpl;

    @Mock
    private MusicianRepository musicianRepository;

    @Test
    void executeTest(@Random(size = 2, type = Musician.class) final List<Musician> musicians) {

        when(this.musicianRepository.getAll()).thenReturn(musicians);

        final List<Musician> result = this.getAllMusiciansImpl.execute();

        assertThat(result).isEqualTo(musicians);

    }
}
