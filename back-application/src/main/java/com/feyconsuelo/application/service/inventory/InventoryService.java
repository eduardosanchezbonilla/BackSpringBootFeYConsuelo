package com.feyconsuelo.application.service.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryRequest;
import com.feyconsuelo.domain.model.inventory.InventoryResponse;

import java.util.List;
import java.util.Optional;

public interface InventoryService {

    void delete(Long inventoryId);

    void logicalDelete(Long inventoryId);

    List<InventoryResponse> getAll();

    Optional<InventoryResponse> get(Long inventoryId);

    void insert(InventoryRequest inventoryRequest);

    void update(Long inventoryId, InventoryRequest inventoryRequest);

}
