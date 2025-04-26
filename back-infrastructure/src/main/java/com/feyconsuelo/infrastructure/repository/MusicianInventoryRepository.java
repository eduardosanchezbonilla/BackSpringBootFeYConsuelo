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

    @Query(value = """
             select json_build_object(
                        'id',m.id,
                        'dni',m.dni,
                        'name',Trim(m.name),
                        'surname',Trim(m.surname),
                        'direction',Trim(m.direction),
                        'municipality',Trim(m.municipality),
                        'province',Trim(m.province),
                        'email',Trim(m.email),
                        'voice',json_build_object(
                                    'id',v.id,
                                    'name',v.name,
                                    'order',v.voice_order
                                ),
                        'image',m.image_thumbnail,
                        'deleteDate',m.delete_date,
                        'birthDate',m.birth_Date,
                        'registrationDate',m.registration_Date,
                        'unregistrationDate',m.unregistration_Date,
                        'dateLastNotificationNonAssistsStreakRehearsals',m.date_Last_Notification_Non_Assists_Streak_Rehearsals,
                        'inventoryObservations',m.inventory_Observations,
                        'phoneNumber',m.phoneNumber,
                        'unregistred',m.unregistred,
                        'observations',m.observations
                      )
            from feyconsuelo.musician_inventory mi,
                 feyconsuelo.musician m,
                 feyconsuelo.voice v
            where mi.musician_id = m.id
              and m.voice_id = v.id
              and mi.inventory_id = :inventoryId
              and mi.delete_date is null
            """,
            nativeQuery = true)
    List<String> findMusiciansWithInventoryProjection(Long inventoryId);

}


