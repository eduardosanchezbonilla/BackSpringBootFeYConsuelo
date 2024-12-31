package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.application.usecase.voice.GetVoiceImpl;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianRequest;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import io.github.glytching.junit.extension.random.Random;
import io.github.glytching.junit.extension.random.RandomBeansExtension;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class UpdateMusicianImplTest {

    @InjectMocks
    private UpdateMusicianImpl updateMusicianImpl;

    @Mock
    private MusicianService musicianService;

    @Mock
    private InsertMusicianImpl insertMusician;

    @Mock
    private DeleteMusicianImpl deleteMusician;

    @Mock
    private UserService userService;

    @Mock
    private GetVoiceImpl getVoice;

    @Mock
    private ResizeImageImpl resizeImageService;

    @Test
    void executeTest(
            @Random final Long musicianId,
            @Random final MusicianRequest musicianRequest,
            @Random final MusicianResponse musicianResponse
    ) {

        when(this.musicianService.get(musicianId)).thenReturn(Optional.of(musicianResponse));

        when(this.musicianService.update(Mockito.any(), Mockito.any())).thenReturn(musicianResponse);

        when(this.getVoice.execute(Mockito.any())).thenReturn(Optional.of(musicianResponse.getVoice()));

        final MusicianResponse result = this.updateMusicianImpl.execute(musicianId, musicianRequest);

        assertThat(result).isEqualTo(musicianResponse);

    }

    @Test
    void executeNotFoundTest(
            @Random final Long musicianId,
            @Random final MusicianRequest musicianRequest
    ) {

        when(this.musicianService.get(musicianId))
                .thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> this.updateMusicianImpl.execute(musicianId, musicianRequest));
    }
}
