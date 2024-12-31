package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryEntity;
import com.feyconsuelo.infrastructure.entities.musicianinventory.MusicianInventoryPK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface MusicianInventoryRepository extends JpaRepository<MusicianInventoryEntity, MusicianInventoryPK> {

    @Query("""
             SELECT musicianInventoryEntity
             FROM MusicianInventoryEntity musicianInventoryEntity
             WHERE musicianInventoryEntity.deleteDate Is Null
             ORDER BY musicianInventoryEntity.id.musicianId
            """)
    List<MusicianInventoryEntity> findAllActives();

    @Query("""
             SELECT musicianInventoryEntity
             FROM MusicianInventoryEntity musicianInventoryEntity
             WHERE musicianInventoryEntity.deleteDate Is Null
               And musicianInventoryEntity.id.musicianId = :musicianId
            """)
    List<MusicianInventoryEntity> findAllActivesByMusician(Long musicianId);

    @Query("""
             SELECT musicianInventoryEntity
             FROM MusicianInventoryEntity musicianInventoryEntity
             WHERE musicianInventoryEntity.deleteDate Is Null
               And musicianInventoryEntity.id.musicianId = :musicianId
               And musicianInventoryEntity.id.inventoryId = :inventoryId
            """)
    Optional<MusicianInventoryEntity> findMusicianInventoryById(
            Long musicianId,
            Long inventoryId
    );

    // And musician.deleteDate Is Null no pongo esta ccondicione pq deben aparecer componentes eliminados para el inventario, si es que tienen algo
    @Query("""
             SELECT musicianInventoryEntity
             From MusicianInventoryEntity musicianInventoryEntity,
                  MusicianEntity musician
             WHERE musicianInventoryEntity.id.musicianId = musician.id
               And musicianInventoryEntity.id.inventoryId = :inventoryId
               And musicianInventoryEntity.deleteDate Is Null               
            """)
    List<MusicianInventoryEntity> findMusiciansWithInventory(Long inventoryId);

}


