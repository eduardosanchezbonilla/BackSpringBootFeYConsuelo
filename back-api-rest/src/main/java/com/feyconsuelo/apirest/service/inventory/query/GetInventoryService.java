package com.feyconsuelo.apirest.service.inventory.query;

import com.feyconsuelo.apirest.converter.inventory.InventoryResponseListToInventoryResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.inventory.InventoryResponseToInventoryResponseDtoConverter;
import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.domain.usecase.inventory.GetAllInventories;
import com.feyconsuelo.domain.usecase.inventory.GetInventory;
import com.feyconsuelo.openapi.model.InventoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetInventoryService {

    private final GetAllInventories getAllInventories;

    private final GetInventory getInventory;

    private final InventoryResponseToInventoryResponseDtoConverter inventoryResponseToInventoryResponseDtoConverter;

    private final InventoryResponseListToInventoryResponseDtoListConverter inventoryResponseListToInventoryResponseDtoListConverter;

    public ResponseEntity<List<InventoryResponseDto>> getAllInventories() {
        final List<InventoryResponse> inventoryResponseList = this.getAllInventories.execute();
        if (CollectionUtils.isEmpty(inventoryResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.inventoryResponseListToInventoryResponseDtoListConverter.convert(inventoryResponseList));
    }

    public ResponseEntity<InventoryResponseDto> getInventory(final Long inventoryId) {
        final Optional<InventoryResponseDto> inventoryResponseDto = this.getInventory.execute(inventoryId).map(this.inventoryResponseToInventoryResponseDtoConverter::convert);
        return inventoryResponseDto.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
