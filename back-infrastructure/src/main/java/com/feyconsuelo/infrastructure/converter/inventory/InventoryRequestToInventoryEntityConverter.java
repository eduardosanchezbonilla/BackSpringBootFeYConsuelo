package com.feyconsuelo.infrastructure.converter.inventory;

import com.feyconsuelo.domain.model.inventory.InventoryRequest;
import com.feyconsuelo.infrastructure.entities.inventory.InventoryEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class InventoryRequestToInventoryEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    @Value("${default-images.inventory}")
    private String defaultInventoryImage;

    private String getInventoryImage(final InventoryRequest inventoryRequest) {
        if (StringUtils.isEmpty(inventoryRequest.getImage())) {
            return inventoryRequest.getImage();
        } else {
            if (inventoryRequest.getImage().equals(this.defaultInventoryImage)) {
                return null;
            } else {
                return inventoryRequest.getImage();
            }
        }
    }

    public InventoryEntity convert(final InventoryRequest inventoryRequest) {
        return InventoryEntity.builder()
                .order(inventoryRequest.getOrder())
                .name(inventoryRequest.getName())
                .units(inventoryRequest.getUnits())
                .image(this.getInventoryImage(inventoryRequest))
                .modifiedUser(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public InventoryEntity updateEntity(final InventoryEntity inventoryEntity,
                                        final InventoryRequest inventoryRequest) {
        inventoryEntity.setOrder(inventoryRequest.getOrder());
        inventoryEntity.setName(inventoryRequest.getName());
        inventoryEntity.setUnits(inventoryRequest.getUnits());
        inventoryEntity.setImage(this.getInventoryImage(inventoryRequest));
        inventoryEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return inventoryEntity;
    }

    public InventoryEntity deleteEntity(final InventoryEntity inventoryEntity) {
        inventoryEntity.setDeleteDate(LocalDateTime.now());
        inventoryEntity.setModifiedUser(this.tokenInfoExtractorService.getUsername());

        return inventoryEntity;
    }
}
