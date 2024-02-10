package com.feyconsuelo.application.usecase;

import com.feyconsuelo.application.repository.MusicianRepository;
import com.feyconsuelo.domain.entity.musician.Musician;
import com.feyconsuelo.domain.exception.FeYConsueloNotFoundException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DeleteMusicianImplTest {

    @InjectMocks
    private DeleteMusicianImpl deleteMusicianImpl;

    @Mock
    private MusicianRepository musicianRepository;

    @Test
    void deleteTest() {

        when(this.musicianRepository.get("id")).thenReturn(Optional.of(Musician.builder().id("1").build()));

        this.deleteMusicianImpl.execute("id");

        verify(this.musicianRepository, times(1)).get("id");
        verify(this.musicianRepository, times(1)).delete("1");

    }

    @Test
    void deleteIdNotFoundTest() {

        when(this.musicianRepository.get("id")).thenReturn(Optional.empty());

        assertThrows(FeYConsueloNotFoundException.class, () -> this.deleteMusicianImpl.execute("id"));

    }


}
