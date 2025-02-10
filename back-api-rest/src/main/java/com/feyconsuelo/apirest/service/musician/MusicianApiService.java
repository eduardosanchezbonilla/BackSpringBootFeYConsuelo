package com.feyconsuelo.apirest.service.musician;

import com.feyconsuelo.apirest.service.musician.changeexpiredpassword.MusicianChangeExpiredPasswordService;
import com.feyconsuelo.apirest.service.musician.delete.DeleteMusicianService;
import com.feyconsuelo.apirest.service.musician.insert.InsertMusicianService;
import com.feyconsuelo.apirest.service.musician.query.GetMusicianService;
import com.feyconsuelo.apirest.service.musician.resetpassword.MusicianResetPasswordService;
import com.feyconsuelo.apirest.service.musician.update.UpdateMusicianService;
import com.feyconsuelo.domain.model.musician.MusicianGroupByVoiceRequest;
import com.feyconsuelo.openapi.api.MusicianControllerApiDelegate;
import com.feyconsuelo.openapi.model.MusicianChangeExpiredPasswordRequestDto;
import com.feyconsuelo.openapi.model.MusicianGroupByVoiceResponseDto;
import com.feyconsuelo.openapi.model.MusicianRequestDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianApiService implements MusicianControllerApiDelegate {

    private final DeleteMusicianService deleteMusicianService;
    private final InsertMusicianService insertMusicianService;
    private final UpdateMusicianService updateMusicianService;
    private final GetMusicianService getMusicianService;
    private final MusicianChangeExpiredPasswordService musicianChangeExpiredPasswordService;
    private final MusicianResetPasswordService resetPasswordService;

    @Override
    public ResponseEntity<Void> deleteMusician(final Long musicianId) {
        return this.deleteMusicianService.deleteMusician(musicianId);
    }

    @Override
    public ResponseEntity<MusicianResponseDto> postMusician(final MusicianRequestDto musicianRequestDto) {
        return this.insertMusicianService.postMusician(musicianRequestDto);
    }

    @Override
    public ResponseEntity<MusicianResponseDto> updateMusician(final Long musicianId,
                                                              final MusicianRequestDto musicianRequestDto) {
        return this.updateMusicianService.updateMusician(musicianId, musicianRequestDto);
    }


    @Override
    public ResponseEntity<List<MusicianResponseDto>> getAllMusicians(final Boolean unregistred) {
        return this.getMusicianService.getAllMusicians(unregistred == null ? Boolean.FALSE : unregistred);
    }

    @Override
    public ResponseEntity<MusicianResponseDto> getMusician(final Long musicianId) {
        return this.getMusicianService.getMusician(musicianId);
    }

    @Override
    public ResponseEntity<MusicianResponseDto> getMusicianFromDni(final String musicianDni) {
        return this.getMusicianService.getMusicianFromDni(musicianDni.toUpperCase());
    }

    @Override
    public ResponseEntity<List<MusicianGroupByVoiceResponseDto>> getMusiciansGroupByVoice(final String name, final Boolean unregistred) {
        return this.getMusicianService.getMusiciansGroupByVoice(
                MusicianGroupByVoiceRequest.builder()
                        .name(name)
                        .unregistred(unregistred == null ? Boolean.FALSE : unregistred)
                        .build()
        );
    }

    @Override
    public ResponseEntity<MusicianResponseDto> changeExpiredPasswordMusician(final String dni,
                                                                             final MusicianChangeExpiredPasswordRequestDto musicianChangeExpiredPasswordRequestDto) {
        return this.musicianChangeExpiredPasswordService.changeExpiredPassword(dni, musicianChangeExpiredPasswordRequestDto);
    }

    @Override
    public ResponseEntity<Void> resetPasswordMusician(final String dni) {
        return this.resetPasswordService.resetPassword(dni);
    }

}
