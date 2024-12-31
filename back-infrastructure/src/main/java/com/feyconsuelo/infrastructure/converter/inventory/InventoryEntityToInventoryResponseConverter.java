package com.feyconsuelo.infrastructure.converter.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.infrastructure.entities.inventory.Inventory;
import com.feyconsuelo.infrastructure.entities.inventory.InventoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class InventoryEntityToInventoryResponseConverter {

    public InventoryResponse convert(final InventoryEntity inventoryEntity) {
        return InventoryResponse.builder()
                .id(inventoryEntity.getId())
                .order(inventoryEntity.getOrder())
                .name(inventoryEntity.getName())
                .units(inventoryEntity.getUnits())
                .image(inventoryEntity.getImage())
                .deleteDate(inventoryEntity.getDeleteDate())
                .build();
    }

    public InventoryResponse convertInventory(final Inventory inventoryEntity) {
        return InventoryResponse.builder()
                .id(inventoryEntity.getId())
                .order(inventoryEntity.getOrder())
                .name(inventoryEntity.getName())
                .image(inventoryEntity.getImage())
                .units(inventoryEntity.getUnits())
                .deleteDate(inventoryEntity.getDeleteDate())
                .musicianWithElement(inventoryEntity.getMusicians())
                .build();
    }
}
