package com.feyconsuelo.apirest.service.inventory.insert;

import com.feyconsuelo.apirest.converter.inventory.InventoryRequestDtoToInventoryRequestConverter;
import com.feyconsuelo.domain.usecase.inventory.InsertInventory;
import com.feyconsuelo.openapi.model.InventoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertInventoryService {

    private final InsertInventory insertInventory;

    private final InventoryRequestDtoToInventoryRequestConverter inventoryRequestDtoToInventoryRequestConverter;

    public ResponseEntity<Void> postInventory(final InventoryRequestDto inventoryRequestDto) {
        this.insertInventory.execute(
                this.inventoryRequestDtoToInventoryRequestConverter.convert(inventoryRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
