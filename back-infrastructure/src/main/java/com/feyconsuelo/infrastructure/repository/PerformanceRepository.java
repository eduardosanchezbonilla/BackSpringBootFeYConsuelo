package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceMusiciansProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface PerformanceRepository extends JpaRepository<PerformanceEntity, Long> {

    @Query("""
             SELECT performanceEntity
             FROM PerformanceEntity performanceEntity
             WHERE performanceEntity.deleteDate Is Null
                And (performanceEntity.date >= :startDate Or :allStartDate = true)
                And (performanceEntity.date <= :endDate Or :allEndDate = true)
             ORDER BY performanceEntity.id
            """)
    List<PerformanceEntity> findAllActivesWithImages(LocalDate startDate,
                                                     LocalDate endDate,
                                                     Boolean allStartDate,
                                                     Boolean allEndDate
    );

    @Query("""
             SELECT performanceEntity.id as performanceId,
                   performanceEntity.date as date,
                   performanceEntity.startTime as startTime,
                   performanceEntity.endTime as endTime,
                   performanceEntity.title as title,
                   performanceEntity.description as description,
                   performanceEntity.performanceType as performanceType,
                   performanceEntity.voiceIdList as voiceIdList,
                   performanceEntity.location as location,
                   performanceEntity.municipality as municipality,
                   performanceEntity.province as province,    
                   performanceEntity.imageThumbnail as imageThumbnail,               
                   performanceEntity.bus as displacementBus,
                   performanceEntity.eventPublic as eventPublic,
                   performanceEntity.repertoirePublic as repertoirePublic,
                   performanceEntity.crossheadPublic as crossheadPublic,
                   performanceEntity.busData as busData,
                   performanceEntity.busTime as busTime,
                   performanceEntity.busLocation as busLocation,
                   performanceEntity.route as route,
                   performanceEntity.currentLat as currentLatitude,
                   performanceEntity.currentLng as currentLongitude,
                   performanceEntity.currentMarch as currentMarch,
                   performanceEntity.duration as duration,
                   performanceEntity.kilometers as kilometers,
                   performanceEntity.googleId as googleId
             FROM PerformanceEntity performanceEntity
             WHERE performanceEntity.deleteDate Is Null
                And (performanceEntity.date >= :startDate Or :allStartDate = true)
                And (performanceEntity.date <= :endDate Or :allEndDate = true)
             ORDER BY performanceEntity.id
            """)
    List<MusicianPerformanceProjection> findAllActivesWithoutImages(LocalDate startDate,
                                                                    LocalDate endDate,
                                                                    Boolean allStartDate,
                                                                    Boolean allEndDate
    );

    @Query("""
             SELECT performanceEntity
             FROM PerformanceEntity performanceEntity
             WHERE performanceEntity.deleteDate Is Null
               And performanceEntity.id = :performanceId
            """)
    Optional<PerformanceEntity> findPerformanceActiveById(Long performanceId);

    @Query("""
             SELECT performanceEntity
             FROM PerformanceEntity performanceEntity
             WHERE performanceEntity.deleteDate Is Null
                And performanceEntity.date = :date
            """)
    Optional<PerformanceEntity> findPerformanceActiveByDate(LocalDate date);


    @Query(value = """
             SELECT p.id as performanceId,
                   p.date as date,
                   p.start_time as startTime,
                   p.end_time as endTime,
                   p.title as title,
                   p.description as description,
                   p.performance_type as performanceType,
                   p.voice_id_list as voiceIdList,
                   p.location as location,
                   p.municipality as municipality,
                   p.province as province,
                   p.image_thumbnail as imageThumbnail,
                   p.bus as displacementBus,
                   p.event_public as eventPublic,
                   p.repertoire_public as repertoirePublic,
                   p.crosshead_public as crossheadPublic,
                   p.bus_data as busData,
                   p.bus_time as busTime,
                   p.bus_location as busLocation,
                   p.route as route,
                   p.current_latitude as currentLatitude,
                   p.current_longitude as currentLongitude,
                   p.current_march as currentMarch,
                   p.duration as duration,
                   p.kilometers as kilometers,
                   p.google_id as googleId,
                   (
                     SELECT json_agg(
                               json_build_object(
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
                                 'observations',m.observations,
                                 'assistLastRehearsal', (
                                                        CASE
                                                           WHEN mp.musician_id IS NOT NULL THEN true
                                                           ELSE false
                                                        END
                                                       ),
                                 'assistBus', (
                                                        CASE
                                                           WHEN mp.bus IS TRUE THEN true
                                                           ELSE false
                                                        END
                                                       ),
                                 'dateLastRehearsal',p.date,
                                 'idLastRehearsal',p.id,
                                 'formationPositionX',mp.formation_x_Position,
                                 'formationPositionY',mp.formation_y_Position
                               )
                             )
                     FROM feyconsuelo.voice AS v,
                         feyconsuelo.musician AS m
                     Left Join feyconsuelo.musician_performance mp
                        On (
                            p.id = mp.performance_id and
                            mp.musician_id = m.id and
                            mp.delete_date is null and
                            mp.musician_id >=0
                           )
                     WHERE m.voice_id = v.id
                        and m.delete_date is null
                       And (m.unregistration_date Is Null Or m.unregistration_date > p.date)
                       And CAST(m.registration_date AS DATE) <= CAST(p.date AS DATE)
                    ) AS musicians,
                    (
                     SELECT json_agg(
                               json_build_object(
                                 'id',mp.musician_id,
                                 'dni','',
                                 'name','Hueco',
                                 'surname','',
                                 'direction','',
                                 'municipality','',
                                 'province','',
                                 'email','',
                                 'voice',json_build_object(
                                                    'id','-1',
                                                    'name','HUECO',
                                                    'order','0'
                                                ),
                                 'image','',
                                 'deleteDate',null,
                                 'birthDate',null,
                                 'registrationDate',null,
                                 'unregistrationDate',null,
                                 'dateLastNotificationNonAssistsStreakRehearsals',null,
                                 'inventoryObservations',null,
                                 'phoneNumber',null,
                                 'unregistred',null,
                                 'observations',null,
                                 'assistLastRehearsal', true,
                                 'assistBus', false,
                                 'dateLastRehearsal',p.date,
                                 'idLastRehearsal',p.id,
                                 'formationPositionX',mp.formation_x_Position,
                                 'formationPositionY',mp.formation_y_Position
                               )
                             )
                     FROM feyconsuelo.musician_performance mp       
                     WHERE mp.delete_date is null
                       and mp.musician_id <0
                        and mp.performance_Id = :performanceId
                    ) AS fakeMusicians
             FROM feyconsuelo.performance p
             WHERE p.delete_Date Is Null
                And p.id = :performanceId            
            """,
            nativeQuery = true)
    Optional<PerformanceMusiciansProjection> findPerformanceMusicians(Long performanceId);

}
