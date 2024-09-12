package com.feyconsuelo.apirest.service.musician;

import com.feyconsuelo.apirest.service.musician.delete.DeleteMusicianService;
import com.feyconsuelo.apirest.service.musician.insert.InsertMusicianService;
import com.feyconsuelo.apirest.service.musician.query.GetMusicianService;
import com.feyconsuelo.apirest.service.musician.update.UpdateMusicianService;
import com.feyconsuelo.openapi.api.MusicianControllerApiDelegate;
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
    public ResponseEntity<List<MusicianResponseDto>> getAllMusicians() {
        return this.getMusicianService.getAllMusicians();
    }

    @Override
    public ResponseEntity<MusicianResponseDto> getMusician(final Long musicianId) {
        return this.getMusicianService.getMusician(musicianId);
    }

}
