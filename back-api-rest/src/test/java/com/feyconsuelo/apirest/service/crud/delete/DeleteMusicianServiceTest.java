package com.feyconsuelo.apirest.service.crud.delete;

import com.feyconsuelo.domain.usecase.DeleteMusician;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class DeleteMusicianServiceTest {

    @InjectMocks
    private DeleteMusicianService deleteMusicianService;

    @Mock
    private DeleteMusician deleteMusician;

    @Test
    void deleteMusicianTest() {

        doNothing().when(this.deleteMusician).execute("id");

        final ResponseEntity<Void> response = this.deleteMusicianService.deleteMusician("id");

        verify(this.deleteMusician, times(1)).execute("id");
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.NO_CONTENT);

    }
}
