package com.feyconsuelo.apirest.converter.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.openapi.model.InventoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class InventoryResponseListToInventoryResponseDtoListConverter {

    private final InventoryResponseToInventoryResponseDtoConverter inventoryResponseToInventoryResponseDtoConverter;

    public List<InventoryResponseDto> convert(final List<InventoryResponse> inventoryResponseList) {
        if (CollectionUtils.isEmpty(inventoryResponseList)) {
            return List.of();
        }
        return inventoryResponseList.stream()
                .map(this.inventoryResponseToInventoryResponseDtoConverter::convert)
                .sorted(Comparator.comparing(InventoryResponseDto::getOrder))
                .toList();
    }

}
