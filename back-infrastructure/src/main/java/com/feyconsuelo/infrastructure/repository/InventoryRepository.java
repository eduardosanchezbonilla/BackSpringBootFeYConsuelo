package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.inventory.Inventory;
import com.feyconsuelo.infrastructure.entities.inventory.InventoryEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface InventoryRepository extends JpaRepository<InventoryEntity, Long> {

    // And musician.deleteDate Is Null no pongo esta condicion pq deben contabilizar componentes borrados si tienen inventario
    @Query("""
             SELECT inventory.id as id,
                    inventory.order as order,
                    inventory.units as units,
                    inventory.image as image,
                    inventory.name as name,
                    inventory.deleteDate as deleteDate,
                    (
                      Select count(1) 
                      From MusicianInventoryEntity musicianInventory,
                           MusicianEntity musician 
                      Where musicianInventory.id.musicianId = musician.id
                         And musicianInventory.id.inventoryId = inventory.id                         
                         And musicianInventory.deleteDate Is Null
                    ) as musicians
             FROM InventoryEntity inventory
             WHERE inventory.deleteDate Is Null
             ORDER BY inventory.id
            """)
    List<Inventory> findAllActives();

    @Query("""
             SELECT inventory
             FROM InventoryEntity inventory
             WHERE inventory.deleteDate Is Null
               And inventory.id = :inventoryId
            """)
    Optional<InventoryEntity> findInventoryActiveById(Long inventoryId);

}
