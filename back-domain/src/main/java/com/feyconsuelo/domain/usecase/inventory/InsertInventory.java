package com.feyconsuelo.domain.usecase.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryRequest;

public interface InsertInventory {

    void execute(InventoryRequest inventoryRequest);

}
