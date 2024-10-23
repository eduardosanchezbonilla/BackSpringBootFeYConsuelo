package com.feyconsuelo.apirest.service.inventory.delete;

import com.feyconsuelo.domain.usecase.inventory.DeleteInventory;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteInventoryService {

    private final DeleteInventory deleteInventory;

    public ResponseEntity<Void> deleteInventory(final Long inventoryId) {
        this.deleteInventory.execute(inventoryId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
