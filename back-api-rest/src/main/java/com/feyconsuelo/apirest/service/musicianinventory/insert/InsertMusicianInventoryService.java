package com.feyconsuelo.apirest.service.musicianinventory.insert;

import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;
import com.feyconsuelo.domain.usecase.musicianinventory.InsertMusicianInventory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertMusicianInventoryService {

    private final InsertMusicianInventory insertMusicianInventory;

    public ResponseEntity<Void> insertMusicianInventory(final Long musicianId,
                                                        final Long inventoryId) {
        this.insertMusicianInventory.execute(
                MusicianInventoryRequest.builder()
                        .musicianId(musicianId)
                        .inventoryId(inventoryId)
                        .build()
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
