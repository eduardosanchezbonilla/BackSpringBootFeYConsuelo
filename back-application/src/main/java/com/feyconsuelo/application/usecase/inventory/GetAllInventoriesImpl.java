package com.feyconsuelo.application.usecase.inventory;

import com.feyconsuelo.application.service.inventory.InventoryService;
import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.domain.usecase.inventory.GetAllInventories;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllInventoriesImpl implements GetAllInventories {

    private final InventoryService inventoryService;

    @Override
    public List<InventoryResponse> execute() {
        return this.inventoryService.getAll();
    }
}
