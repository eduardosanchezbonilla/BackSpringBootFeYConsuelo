package com.feyconsuelo.apirest.service.musicianinventory.query;

import com.feyconsuelo.apirest.converter.musician.MusicianResponseListToMusicianResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.musicianinventory.MusicianInventoryResponseListToMusicianInventoryResponseDtoListConverter;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;
import com.feyconsuelo.domain.usecase.musicianinventory.GetMusicianInventories;
import com.feyconsuelo.domain.usecase.musicianinventory.GetMusiciansWithInventoryAssociated;
import com.feyconsuelo.openapi.model.MusicianInventoryResponseDto;
import com.feyconsuelo.openapi.model.MusicianResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetMusicianInventoryService {

    private final GetMusicianInventories getMusicianInventories;

    private final GetMusiciansWithInventoryAssociated getMusiciansWithInventoryAssociated;

    private final MusicianInventoryResponseListToMusicianInventoryResponseDtoListConverter musicianInventoryResponseListToMusicianInventoryResponseDtoListConverter;

    private final MusicianResponseListToMusicianResponseDtoListConverter musicianResponseListToMusicianResponseDtoListConverter;

    public ResponseEntity<List<MusicianInventoryResponseDto>> getMusicianInventories(final Long musicianId) {
        final List<MusicianInventoryResponse> inventories = this.getMusicianInventories.execute(musicianId);
        if (CollectionUtils.isEmpty(inventories)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.musicianInventoryResponseListToMusicianInventoryResponseDtoListConverter.convert(inventories));
    }

    public ResponseEntity<List<MusicianResponseDto>> getMusicianWithInventoryAssociated(final Long inventoryId) {
        final List<MusicianResponse> musicians = this.getMusiciansWithInventoryAssociated.execute(inventoryId);
        if (CollectionUtils.isEmpty(musicians)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.musicianResponseListToMusicianResponseDtoListConverter.convert(musicians));
    }

}
