package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformancePK;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;

public interface MusicianPerformanceRepository extends JpaRepository<MusicianPerformanceEntity, MusicianPerformancePK> {

    @Query("""
             SELECT musicianPerformanceEntity
             FROM MusicianPerformanceEntity musicianPerformanceEntity
             WHERE musicianPerformanceEntity.deleteDateMP Is Null
                And musicianPerformanceEntity.musician.id = :musicianId
                And (musicianPerformanceEntity.performance.date >= :startDate Or :allStartDate = true)
                And (musicianPerformanceEntity.performance.date <= :endDate  Or :allEndDate = true)
                And musicianPerformanceEntity.id.musicianId >= 0
             ORDER BY musicianPerformanceEntity.performance.date
            """)
    List<MusicianPerformanceEntity> findAllActives(
            Long musicianId,
            LocalDate startDate,
            LocalDate endDate,
            Boolean allStartDate,
            Boolean allEndDate
    );

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
                   mp.musician_id as musicianId,
                   mp.formation_x_position as formationPositionX,
                   mp.formation_y_position as formationPositionY,
                   mp.bus as musicianBus
             FROM feyconsuelo.performance p
             Left Join feyconsuelo.musician_performance mp
                On (
                    p.id = mp.performance_id and
                    mp.musician_id = :musicianId and
                    mp.delete_date is null and
                    mp.musician_id >=0	   
                   )
             WHERE p.delete_Date Is Null
                And (p.date >= :startDate Or :allStartDate is true)
                And (p.date <= :endDate Or :allEndDate is true)
             ORDER BY p.id
            """,
            nativeQuery = true)
    List<MusicianPerformanceProjection> findAllMusicianPerformanceActives(
            Long musicianId,
            LocalDate startDate,
            LocalDate endDate,
            Boolean allStartDate,
            Boolean allEndDate
    );

    @Query("""
             SELECT musicianPerformanceEntity.id.performanceId as performanceId,
                    musicianPerformanceEntity.id.musicianId as musicianId,
                    musicianPerformanceEntity.formationPositionX as formationPositionX,
                    musicianPerformanceEntity.formationPositionY as formationPositionY,
                    musicianPerformanceEntity.performance as performance
             FROM MusicianPerformanceEntity musicianPerformanceEntity
             WHERE musicianPerformanceEntity.deleteDateMP Is Null
                And musicianPerformanceEntity.id.performanceId = :performanceId
                And musicianPerformanceEntity.id.musicianId < 0
            """)
    List<MusicianPerformanceProjection> findAllActivesFakeMusiciansByPerformanceId(Long performanceId);

    @Modifying
    @Query("""
            DELETE FROM MusicianPerformanceEntity musicianPerformanceEntity
            WHERE musicianPerformanceEntity.id.musicianId < 0
               And musicianPerformanceEntity.id.performanceId = :performanceId
            """)
    void deleteFakeMusicians(Long performanceId);

    @Query("""
             SELECT musicianPerformanceEntity
             FROM MusicianPerformanceEntity musicianPerformanceEntity
             WHERE musicianPerformanceEntity.deleteDateMP Is Null
                And musicianPerformanceEntity.id.performanceId = :performanceId
                And musicianPerformanceEntity.id.musicianId >= 0
            """)
    List<MusicianPerformanceEntity> findAllActivesMusiciansByPerformanceId(Long performanceId);

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
                   mp.musician_id as musicianId,
                   mp.formation_x_position as formationPositionX,
                   mp.formation_y_position as formationPositionY,
                   mp.bus as musicianBus,
                   m.dni as musicianDni,
                   m.name as musicianName,
                   m.surname as musicianSurname,
                   m.direction as musicianDirection,
                   m.municipality as musicianMunicipality,
                   m.province as musicianProvince,
                   m.email as musicianEmail,
                   v.id as voiceId,
                   v.voice_order as voiceOrder,
                   v.name as voiceName,      
                   m.image_thumbnail as musicianImage,
                   m.delete_date as musicianDeleteDate,
                   m.birth_date as musicianBirthDate,
                   m.registration_date as musicianRegistrationDate,
                   m.unregistration_date as musicianUnregistrationDate,
                   m.unregistred as musicianUnregistred,
                   m.date_last_notification_non_assists_streak_rehearsals  as musicianDateLastNotificationNonAssistsStreakRehearsals,
                   m.inventory_observations as musicianInventoryObservations,
                   m.phonenumber as musicianPhoneNumber,
                   m.observations as musicianObservations
             FROM feyconsuelo.performance p,
                  feyconsuelo.musician_performance mp,
                  feyconsuelo.musician m,
                  feyconsuelo.voice v
             WHERE p.id = mp.performance_id
                and m.id = mp.musician_id
                and m.voice_id = v.id
                and mp.delete_date is null
                and mp.musician_id >=0	  
                and p.delete_Date Is Null
                And p.id = :performanceId
             ORDER BY p.id
            """
            , nativeQuery = true)
    List<MusicianPerformanceProjection> findAllActivesMusiciansByPerformanceId1(Long performanceId);


}
