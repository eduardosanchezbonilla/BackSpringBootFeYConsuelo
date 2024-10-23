package com.feyconsuelo.application.usecase.musicianinventory;

import com.feyconsuelo.application.service.musicianinventory.MusicianInventoryService;
import com.feyconsuelo.domain.model.musician.MusicianResponse;
import com.feyconsuelo.domain.usecase.musicianinventory.GetMusiciansWithInventoryAssociated;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetMusiciansWithInventoryAssociatedImpl implements GetMusiciansWithInventoryAssociated {

    private final MusicianInventoryService musicianInventoryService;

    @Override
    public List<MusicianResponse> execute(final Long inventoryId) {
        return this.musicianInventoryService.getMusiciansWithInventory(inventoryId);
    }
}
