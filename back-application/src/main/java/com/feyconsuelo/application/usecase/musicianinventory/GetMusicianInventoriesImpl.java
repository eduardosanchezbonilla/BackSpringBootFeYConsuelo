package com.feyconsuelo.application.usecase.musicianinventory;

import com.feyconsuelo.application.service.inventory.InventoryService;
import com.feyconsuelo.application.service.musician.MusicianService;
import com.feyconsuelo.application.service.musicianinventory.MusicianInventoryService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.model.musicianinventory.MusicianInventoryResponse;
import com.feyconsuelo.domain.usecase.musicianinventory.GetMusicianInventories;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetMusicianInventoriesImpl implements GetMusicianInventories {

    private final InventoryService inventoryService;
    private final MusicianInventoryService musicianInventoryService;
    private final MusicianService musicianService;

    @Override
    public List<MusicianInventoryResponse> execute(final Long musicianId) {

        final Optional<MusicianResponse> musicianResponse = this.musicianService.get(musicianId, Boolean.TRUE);

        if (musicianResponse.isEmpty()) {
            throw new NotFoundException("No existe el musico");
        }

        // obtenemos todos los elementos de inventario
        final List<InventoryResponse> allInventories = this.inventoryService.getAll();

        // obtenemos los elementos de inventario asignados al musico
        final List<MusicianInventoryResponse> musicianInventories = this.musicianInventoryService.getAllMusicianInventories(musicianId);


        if (CollectionUtils.isEmpty(allInventories)) {
            return List.of();
        } else {
            return allInventories.stream()
                    .map(musicianInventory ->
                            MusicianInventoryResponse.builder()
                                    .musicianId(musicianId)
                                    .inventoryId(musicianInventory.getId())
                                    .inventoryName(musicianInventory.getName())
                                    .inventoryImage(musicianInventory.getImage())
                                    .inventoryOrder(musicianInventory.getOrder())
                                    .assigned(musicianInventories.stream().anyMatch(inventory -> inventory.getInventoryId().equals(musicianInventory.getId())))
                                    .build()
                    )
                    .sorted(Comparator.comparing(MusicianInventoryResponse::getInventoryOrder))
                    .toList();
        }
    }
}
