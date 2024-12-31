package com.feyconsuelo.apirest.converter.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryRequest;
import com.feyconsuelo.openapi.model.InventoryRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class InventoryRequestDtoToInventoryRequestConverter {
    public InventoryRequest convert(final InventoryRequestDto inventoryRequestDto) {
        return InventoryRequest.builder()
                .order(inventoryRequestDto.getOrder())
                .name(inventoryRequestDto.getName().toUpperCase())
                .image(inventoryRequestDto.getImage())
                .units(inventoryRequestDto.getUnits())
                .build();
    }

}
