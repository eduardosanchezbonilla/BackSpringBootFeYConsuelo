package com.feyconsuelo.apirest.converter.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.openapi.model.InventoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class InventoryResponseToInventoryResponseDtoConverter {

    public InventoryResponseDto convert(final InventoryResponse inventoryResponse) {
        return InventoryResponseDto.builder()
                .id(inventoryResponse.getId())
                .order(inventoryResponse.getOrder())
                .name(inventoryResponse.getName())
                .units(inventoryResponse.getUnits())
                .image(inventoryResponse.getImage())
                .musicianWithElement(inventoryResponse.getMusicianWithElement())
                .build();
    }

}
