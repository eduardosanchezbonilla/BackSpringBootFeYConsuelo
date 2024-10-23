package com.feyconsuelo.application.usecase.musician;

import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.application.usecase.user.GetUserImpl;
import com.feyconsuelo.application.usecase.user.InsertUserImpl;
import com.feyconsuelo.application.usecase.user.UpdateUserRolesImpl;
import com.feyconsuelo.application.usecase.voice.GetVoiceImpl;
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
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, RandomBeansExtension.class})
class InsertMusicianImplTest {

    @InjectMocks
    private InsertMusicianImpl insertMusicianImpl;

    @Mock
    private MusicianService musicianService;

    @Mock
    private GetUserImpl getUser;

    @Mock
    private InsertUserImpl insertUser;

    @Mock
    private UpdateUserRolesImpl updateUserRoles;

    @Mock
    private GetVoiceImpl getVoice;

    @Mock
    private ResizeImageImpl resizeImageService;

    @Test
    void executeTest(@Random final MusicianRequest musicianRequest,
                     @Random final MusicianResponse musicianResponse) {

        when(this.musicianService.insert(musicianRequest)).thenReturn(musicianResponse);

        when(this.getVoice.execute(Mockito.any())).thenReturn(Optional.of(musicianResponse.getVoice()));

        final MusicianResponse result = this.insertMusicianImpl.execute(musicianRequest);

        assertThat(result).isEqualTo(musicianResponse);

    }
}
