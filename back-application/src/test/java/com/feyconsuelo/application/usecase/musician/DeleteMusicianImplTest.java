package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.user.DeleteUserImpl;
import com.feyconsuelo.application.usecase.user.GetUserImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class DeleteMusicianImplTest {

    @InjectMocks
    private DeleteMusicianImpl deleteMusicianImpl;

    @Mock
    private MusicianService musicianService;

    @Mock
    private DeleteUserImpl deleteUser;

    @Mock
    private GetUserImpl getUser;

    @Test
    void deleteTest() {

        when(this.musicianService.get(1L, true)).thenReturn(Optional.of(MusicianResponse.builder().id(1L).dni("66666666G").build()));

        this.deleteMusicianImpl.execute(1L);

        verify(this.musicianService, times(1)).get(1L, true);
        verify(this.musicianService, times(1)).logicalDelete(Mockito.any());

    }

    @Test
    void deleteIdNotFoundTest() {

        when(this.musicianService.get(1L, true)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> this.deleteMusicianImpl.execute(1L));

    }


}
