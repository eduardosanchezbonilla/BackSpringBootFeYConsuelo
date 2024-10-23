package com.feyconsuelo.application.usecase.inventory;

import com.feyconsuelo.application.service.inventory.InventoryService;
import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.domain.usecase.inventory.GetInventory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetInventoryImpl implements GetInventory {

    private final InventoryService invertoryService;

    @Override
    public Optional<InventoryResponse> execute(final Long inventoryId) {
        return this.invertoryService.get(inventoryId);
    }
}
