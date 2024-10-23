package com.feyconsuelo.apirest.service.inventory;

import com.feyconsuelo.apirest.service.inventory.delete.DeleteInventoryService;
import com.feyconsuelo.apirest.service.inventory.insert.InsertInventoryService;
import com.feyconsuelo.apirest.service.inventory.query.GetInventoryService;
import com.feyconsuelo.apirest.service.inventory.update.UpdateInventoryService;
import com.feyconsuelo.openapi.api.InventoryControllerApiDelegate;
import com.feyconsuelo.openapi.model.InventoryRequestDto;
import com.feyconsuelo.openapi.model.InventoryResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class InventoryApiService implements InventoryControllerApiDelegate {

    private final DeleteInventoryService deleteInventoryService;
    private final InsertInventoryService insertInventoryService;
    private final UpdateInventoryService updateInventoryService;
    private final GetInventoryService getInventoryService;

    @Override
    public ResponseEntity<Void> deleteInventory(final Long voiceId) {
        return this.deleteInventoryService.deleteInventory(voiceId);
    }

    @Override
    public ResponseEntity<Void> postInventory(final InventoryRequestDto voiceRequestDto) {
        return this.insertInventoryService.postInventory(voiceRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateInventory(final Long voiceId,
                                                final InventoryRequestDto voiceRequestDto) {
        return this.updateInventoryService.updateInventory(voiceId, voiceRequestDto);
    }


    @Override
    public ResponseEntity<List<InventoryResponseDto>> getAllInventories() {
        return this.getInventoryService.getAllInventories();
    }

    @Override
    public ResponseEntity<InventoryResponseDto> getInventory(final Long voiceId) {
        return this.getInventoryService.getInventory(voiceId);
    }

}
