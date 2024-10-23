package com.feyconsuelo.domain.usecase.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryRequest;

public interface UpdateInventory {

    void execute(Long inventoryId, InventoryRequest inventoryRequest);

}
