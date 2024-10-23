package com.feyconsuelo.application.usecase.inventory;

import com.feyconsuelo.application.service.inventory.InventoryService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.inventory.InventoryRequest;
import com.feyconsuelo.domain.usecase.inventory.UpdateInventory;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateInventoryImpl implements UpdateInventory {

    private final InventoryService inventoryService;
    private final ResizeImageImpl resizeImageService;
    @Value("${default-images.inventory}")
    private String defaultInventoryImage;

    @Override
    public void execute(final Long inventoryId, final InventoryRequest inventoryRequest) {
        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(inventoryRequest.getImage()) && !inventoryRequest.getImage().equals(this.defaultInventoryImage)) {
            inventoryRequest.setImage(this.resizeImageService.resizeImage(inventoryRequest.getImage()));
        }

        this.inventoryService.update(inventoryId, inventoryRequest);
    }

}
