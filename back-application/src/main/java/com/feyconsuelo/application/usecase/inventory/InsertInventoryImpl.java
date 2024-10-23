package com.feyconsuelo.application.usecase.inventory;

import com.feyconsuelo.application.service.inventory.InventoryService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.inventory.InventoryRequest;
import com.feyconsuelo.domain.usecase.inventory.InsertInventory;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertInventoryImpl implements InsertInventory {

    private final InventoryService inventoryService;

    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.inventory}")
    private String defaultInventoryImage;

    @Override
    public void execute(final InventoryRequest inventoryRequest) {

        // si estan enviando imagen y no es la imagen por defecto, debemos redimensionarla
        if (StringUtils.isNotEmpty(inventoryRequest.getImage()) && !inventoryRequest.getImage().equals(this.defaultInventoryImage)) {
            inventoryRequest.setImage(this.resizeImageService.resizeImage(inventoryRequest.getImage()));
        }

        this.inventoryService.insert(inventoryRequest);
    }

}
