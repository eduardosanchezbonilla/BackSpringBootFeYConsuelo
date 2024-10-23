package com.feyconsuelo.apirest.service.inventory.update;

import com.feyconsuelo.apirest.converter.inventory.InventoryRequestDtoToInventoryRequestConverter;
import com.feyconsuelo.domain.usecase.inventory.UpdateInventory;
import com.feyconsuelo.openapi.model.InventoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateInventoryService {

    private final UpdateInventory updateInventory;

    private final InventoryRequestDtoToInventoryRequestConverter inventoryRequestDtoToInventoryRequestConverter;

    public ResponseEntity<Void> updateInventory(final Long inventoryId,
                                                final InventoryRequestDto inventoryRequestDto) {
        this.updateInventory.execute(
                inventoryId,
                this.inventoryRequestDtoToInventoryRequestConverter.convert(inventoryRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
