package com.feyconsuelo.apirest.service.musicianinventory.delete;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;
import com.feyconsuelo.domain.usecase.musicianinventory.DeleteMusicianInventory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteMusicianInventoryService {

    private final DeleteMusicianInventory deleteMusicianInventory;

    public ResponseEntity<Void> deleteMusicianInventory(final Long musicianId,
                                                        final Long inventoryId) {
        this.deleteMusicianInventory.execute(
                MusicianInventoryRequest.builder()
                        .musicianId(musicianId)
                        .inventoryId(inventoryId)
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
