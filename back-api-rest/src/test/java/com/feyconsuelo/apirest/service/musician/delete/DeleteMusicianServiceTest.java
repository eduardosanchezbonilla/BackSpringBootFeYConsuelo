package com.feyconsuelo.apirest.service.musician.delete;

import com.feyconsuelo.domain.usecase.musician.DeleteMusician;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class DeleteMusicianServiceTest {

    @InjectMocks
    private DeleteMusicianService deleteMusicianService;

    @Mock
    private DeleteMusician deleteMusician;

    @Test
    void deleteMusicianTest() {

        doNothing().when(this.deleteMusician).execute(1L);

        final ResponseEntity<Void> response = this.deleteMusicianService.deleteMusician(1L);

        verify(this.deleteMusician, times(1)).execute(1L);
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);

    }
}
