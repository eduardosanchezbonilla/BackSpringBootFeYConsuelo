package com.feyconsuelo.application.usecase.inventory;

import com.feyconsuelo.application.service.inventory.InventoryService;
import com.feyconsuelo.application.service.musicianinventory.MusicianInventoryService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.inventory.DeleteInventory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class DeleteInventoryImpl implements DeleteInventory {

    private final InventoryService inventoryService;
    private final MusicianInventoryService musicianInventoryService;


    @Override
    public void execute(final Long inventoryId) {
        final List<MusicianResponse> musicians = this.musicianInventoryService.getMusiciansWithInventory(inventoryId);
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(musicians))) {
            throw new BadRequestException("No se pueden eliminar elementos de inventario asociados a musicos");
        }
        this.inventoryService.logicalDelete(inventoryId);
    }

}
