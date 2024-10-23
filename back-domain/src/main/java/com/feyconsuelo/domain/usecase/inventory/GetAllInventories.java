package com.feyconsuelo.domain.usecase.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryResponse;

import java.util.List;

public interface GetAllInventories {

    List<InventoryResponse> execute();

}
