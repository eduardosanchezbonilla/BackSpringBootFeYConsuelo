package com.feyconsuelo.infrastructure.converter.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.infrastructure.entities.inventory.Inventory;
import com.feyconsuelo.infrastructure.entities.inventory.InventoryEntity;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class InventoryEntityListToInventoryResponseListConverter {

    private final InventoryEntityToInventoryResponseConverter inventoryEntityToInventoryResponseConverter;

    public List<InventoryResponse> convert(final List<InventoryEntity> inventoryEntityList) {
        if (CollectionUtils.isEmpty(inventoryEntityList)) {
            return List.of();
        }
        return inventoryEntityList.stream()
                .map(this.inventoryEntityToInventoryResponseConverter::convert)
                .toList();
    }

    public List<InventoryResponse> convertInventoryList(final List<Inventory> inventoryEntityList) {
        if (CollectionUtils.isEmpty(inventoryEntityList)) {
            return List.of();
        }
        return inventoryEntityList.stream()
                .map(this.inventoryEntityToInventoryResponseConverter::convertInventory)
                .toList();
    }
}
