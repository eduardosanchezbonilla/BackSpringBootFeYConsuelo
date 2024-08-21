package com.feyconsuelo.apirest.service.crud;

import com.feyconsuelo.apirest.service.crud.delete.DeleteMusicianService;
import com.feyconsuelo.apirest.service.crud.insert.InsertMusicianService;
import com.feyconsuelo.apirest.service.crud.query.GetMusicianService;
import com.feyconsuelo.apirest.service.crud.update.UpdateMusicianService;
import com.feyconsuelo.openapi.api.MusicianControllerApiDelegate;
import com.feyconsuelo.openapi.model.MusicianDTO;
import com.feyconsuelo.openapi.model.MusicianResponseDTO;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianService implements MusicianControllerApiDelegate {

    private final DeleteMusicianService deleteMusicianService;
    private final InsertMusicianService insertMusicianService;
    private final UpdateMusicianService updateMusicianService;
    private final GetMusicianService getMusicianService;

    @Override
    public ResponseEntity<Void> deleteMusician(final String musicianId) {
        return this.deleteMusicianService.deleteMusician(musicianId);
    }

    @Override
    public ResponseEntity<MusicianResponseDTO> postMusician(final MusicianDTO musicianDTO) {
        return this.insertMusicianService.postMusician(musicianDTO);
    }

    @Override
    public ResponseEntity<MusicianResponseDTO> updateMusician(final String musicianId,
                                                              final MusicianDTO musicianDTO) {
        return this.updateMusicianService.updateMusician(musicianId, musicianDTO);
    }


    @Override
    public ResponseEntity<List<MusicianResponseDTO>> getAllMusicians() {
        ResponseEntity<List<MusicianResponseDTO>> responseEntity = this.getMusicianService.getAllMusicians();
        return responseEntity;
    }

    @Override
    public ResponseEntity<MusicianResponseDTO> getMusician(final String musicianId) {
        return this.getMusicianService.getMusician(musicianId);
    }

}
