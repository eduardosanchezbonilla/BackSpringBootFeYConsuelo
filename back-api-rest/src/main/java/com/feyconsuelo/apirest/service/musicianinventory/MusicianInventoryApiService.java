package com.feyconsuelo.apirest.service.musicianinventory;

import com.feyconsuelo.apirest.service.musicianinventory.delete.DeleteMusicianInventoryService;
import com.feyconsuelo.apirest.service.musicianinventory.insert.InsertMusicianInventoryService;
import com.feyconsuelo.apirest.service.musicianinventory.query.GetMusicianInventoryService;
import com.feyconsuelo.openapi.api.MusicianInventoryControllerApiDelegate;
import com.feyconsuelo.openapi.model.MusicianInventoryResponseDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianInventoryApiService implements MusicianInventoryControllerApiDelegate {

    private final DeleteMusicianInventoryService deleteMusicianInventoryService;
    private final InsertMusicianInventoryService insertMusicianInventoryService;
    private final GetMusicianInventoryService getMusicianInventoryService;

    @Override
    public ResponseEntity<Void> insertMusicianInventory(final Long musicianId,
                                                        final Long inventoryId) {
        return this.insertMusicianInventoryService.insertMusicianInventory(musicianId, inventoryId);
    }

    @Override
    public ResponseEntity<Void> deleteMusicianInventory(final Long musicianId,
                                                        final Long inventoryId) {
        return this.deleteMusicianInventoryService.deleteMusicianInventory(musicianId, inventoryId);
    }

    @Override
    public ResponseEntity<List<MusicianInventoryResponseDto>> getMusicianInventory(final Long musicianId) {
        return this.getMusicianInventoryService.getMusicianInventories(musicianId);
    }

    @Override
    public ResponseEntity<List<MusicianResponseDto>> getMusicianWithInventoryAssociated(final Long inventoryId) {
        return this.getMusicianInventoryService.getMusicianWithInventoryAssociated(inventoryId);
    }

}
