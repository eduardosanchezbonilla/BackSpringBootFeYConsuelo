package com.feyconsuelo.domain.usecase.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryResponse;

import java.util.Optional;

public interface GetInventory {

    Optional<InventoryResponse> execute(Long inventoryId);

}
