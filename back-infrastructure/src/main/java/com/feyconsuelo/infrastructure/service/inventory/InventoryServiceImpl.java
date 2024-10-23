package com.feyconsuelo.infrastructure.service.inventory;

import com.feyconsuelo.application.service.inventory.InventoryService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.inventory.InventoryRequest;
import com.feyconsuelo.domain.model.inventory.InventoryResponse;
import com.feyconsuelo.infrastructure.converter.inventory.InventoryEntityListToInventoryResponseListConverter;
import com.feyconsuelo.infrastructure.converter.inventory.InventoryEntityToInventoryResponseConverter;
import com.feyconsuelo.infrastructure.converter.inventory.InventoryRequestToInventoryEntityConverter;
import com.feyconsuelo.infrastructure.entities.inventory.InventoryEntity;
import com.feyconsuelo.infrastructure.repository.InventoryRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class InventoryServiceImpl implements InventoryService {

    private final InventoryRepository inventoryRepository;
    private final InventoryRequestToInventoryEntityConverter inventoryRequestToInventoryEntityConverter;
    private final InventoryEntityListToInventoryResponseListConverter inventoryEntityListToInventoryResponseListConverter;
    private final InventoryEntityToInventoryResponseConverter inventoryEntityToInventoryResponseConverter;

    @Override
    public void delete(final Long inventoryId) {
        this.inventoryRepository.deleteById(inventoryId);
    }

    @Override
    public void logicalDelete(final Long inventoryId) {

        final var inventory = this.inventoryRepository.findInventoryActiveById(inventoryId);

        if (inventory.isEmpty()) {
            throw new NotFoundException("No existe el elemento de inventario que desea eliminar");
        }

        this.inventoryRepository.save(this.inventoryRequestToInventoryEntityConverter.deleteEntity(inventory.get()));
    }

    @Override
    public List<InventoryResponse> getAll() {
        final List<InventoryEntity> inventories = this.inventoryRepository.findAllActives();
        return this.inventoryEntityListToInventoryResponseListConverter.convert(inventories);
    }

    @Override
    public Optional<InventoryResponse> get(final Long inventoryId) {
        final var inventory = this.inventoryRepository.findInventoryActiveById(inventoryId);
        return inventory.map(this.inventoryEntityToInventoryResponseConverter::convert);
    }

    @Override
    public void insert(final InventoryRequest inventoryRequest) {
        this.inventoryRepository.save(
                this.inventoryRequestToInventoryEntityConverter.convert(inventoryRequest)
        );
    }

    @Override
    public void update(final Long inventoryId,
                       final InventoryRequest inventoryRequest) {

        final var inventory = this.inventoryRepository.findInventoryActiveById(inventoryId);

        if (inventory.isEmpty()) {
            throw new NotFoundException("No existe el elemento de inventario que desea modificar");
        }

        inventory.get().setOrder(inventoryRequest.getOrder());
        inventory.get().setName(inventoryRequest.getName());
        inventory.get().setImage(inventoryRequest.getImage());
        this.inventoryRepository.save(
                this.inventoryRequestToInventoryEntityConverter.updateEntity(inventory.get(), inventoryRequest)
        );
    }

}
