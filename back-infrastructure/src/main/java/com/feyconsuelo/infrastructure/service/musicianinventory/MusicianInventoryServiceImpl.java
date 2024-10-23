package com.feyconsuelo.infrastructure.service.musicianinventory;

import com.feyconsuelo.application.service.musicianinventory.MusicianInventoryService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryRequest;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;
import com.feyconsuelo.infrastructure.converter.musicianinventory.MusicianInventoryEntityListToMusicianInventoryResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musicianinventory.MusicianInventoryEntityListToMusicianResponseListConverter;
import com.feyconsuelo.infrastructure.converter.musicianinventory.MusicianInventoryRequestToMusicianInventoryEntityConverter;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryEntity;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryPK;
import com.feyconsuelo.infrastructure.repository.MusicianInventoryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class MusicianInventoryServiceImpl implements MusicianInventoryService {

    private final MusicianInventoryRepository musicianInventoryRepository;
    private final MusicianInventoryRequestToMusicianInventoryEntityConverter musicianInventoryRequestToMusicianInventoryEntityConverter;
    private final MusicianInventoryEntityListToMusicianInventoryResponseListConverter musicianInventoryEntityListToMusicianInventoryResponseListConverter;
    private final MusicianInventoryEntityListToMusicianResponseListConverter musicianInventoryEntityListToMusicianResponseListConverter;

    @Override
    public void insert(final MusicianInventoryRequest musicianInventoryRequest) {
        this.musicianInventoryRepository.save(
                this.musicianInventoryRequestToMusicianInventoryEntityConverter.convert(musicianInventoryRequest)
        );
    }

    @Override
    public void logicalDelete(final MusicianInventoryRequest musicianInventoryRequest) {

        final var musicianInventory = this.musicianInventoryRepository.findMusicianInventoryById(
                musicianInventoryRequest.getMusicianId(),
                musicianInventoryRequest.getInventoryId()
        );

        if (musicianInventory.isEmpty()) {
            throw new NotFoundException("No existe el registro que intenta eliminar");
        }

        musicianInventory.get().setDeleteDate(LocalDateTime.now());
        this.musicianInventoryRepository.save(musicianInventory.get());
    }

    @Override
    public void delete(final MusicianInventoryRequest musicianInventoryRequest) {
        this.musicianInventoryRepository.deleteById(
                MusicianInventoryPK.builder()
                        .musicianId(musicianInventoryRequest.getMusicianId())
                        .inventoryId(musicianInventoryRequest.getInventoryId())
                        .build()
        );
    }

    @Override
    public List<MusicianInventoryResponse> getAllMusicianInventories(final Long musicianId) {
        final List<MusicianInventoryEntity> musicianInventories = this.musicianInventoryRepository.findAllActivesByMusician(musicianId);
        return this.musicianInventoryEntityListToMusicianInventoryResponseListConverter.convert(musicianInventories);
    }

    @Override
    public List<MusicianResponse> getMusiciansWithInventory(final Long inventoryId) {
        final List<MusicianInventoryEntity> musicianInventories = this.musicianInventoryRepository.findMusiciansWithInventory(inventoryId);
        return this.musicianInventoryEntityListToMusicianResponseListConverter.convert(musicianInventories);
    }

}
