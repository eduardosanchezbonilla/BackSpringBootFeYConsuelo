package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.inventory.InventoryEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface InventoryRepository extends JpaRepository<InventoryEntity, Long> {

    @Query("""
             SELECT inventory
             FROM InventoryEntity inventory
             WHERE inventory.deleteDate Is Null
             ORDER BY inventory.id
            """)
    List<InventoryEntity> findAllActives();

    @Query("""
             SELECT inventory
             FROM InventoryEntity inventory
             WHERE inventory.deleteDate Is Null
               And inventory.id = :inventoryId
            """)
    Optional<InventoryEntity> findInventoryActiveById(Long inventoryId);

}
