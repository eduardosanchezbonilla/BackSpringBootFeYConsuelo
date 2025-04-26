package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalPK;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface MusicianRehearsalRepository extends JpaRepository<MusicianRehearsalEntity, MusicianRehearsalPK> {

    @Query("""
             SELECT musicianRehearsalEntity
             FROM MusicianRehearsalEntity musicianRehearsalEntity
             WHERE musicianRehearsalEntity.deleteDateMR Is Null
                And musicianRehearsalEntity.musician.id = :musicianId
                And musicianRehearsalEntity.rehearsal.date >= :startDate
                And musicianRehearsalEntity.rehearsal.date <= :endDate
                And musicianRehearsalEntity.id.musicianId >= 0
             ORDER BY musicianRehearsalEntity.rehearsal.date
            """)
    List<MusicianRehearsalEntity> findAllActives(Long musicianId, LocalDate startDate, LocalDate endDate);

    @Query(value = """
             SELECT r.id as rehearsalId,
                   r.date as date,
                   r.start_time as startTime,
                   r.end_time as endTime,
                   r.description as description,
                   r.voice_id_list as voiceIdList,
                   r.location as location,
                   r.municipality as municipality,
                   r.province as province,
                   r.duration as duration,
                   mr.musician_id as musicianId,
                   mr.formation_x_position as formationPositionX,
                   mr.formation_y_position as formationPositionY
            FROM feyconsuelo.rehearsal r
            Left Join feyconsuelo.musician_rehearsal mr
                On (
                    r.id = mr.rehearsal_id and
                    mr.musician_id = :musicianId and
                    mr.delete_date is null and
                    mr.musician_id >=0
                   )
            WHERE r.delete_Date Is Null
               And (r.date >= :startDate Or :allStartDate is true)
               And (r.date <= :endDate Or :allEndDate is true)
            ORDER BY r.id
            """,
            nativeQuery = true)
    List<MusicianRehearsalProjection> findAllMusicianRehearsalActives(
            Long musicianId,
            LocalDate startDate,
            LocalDate endDate,
            Boolean allStartDate,
            Boolean allEndDate
    );

    @Query("""
             SELECT musicianRehearsalEntity
             FROM MusicianRehearsalEntity musicianRehearsalEntity,
                  (
                   Select Max(rehearsalEntity.startTime) as maxDate
                   From RehearsalEntity rehearsalEntity
                   Where rehearsalEntity.deleteDate Is Null
                      And rehearsalEntity.startTime <= :dateTime
                  ) as maxDate
             WHERE musicianRehearsalEntity.deleteDateMR Is Null
                And musicianRehearsalEntity.rehearsal.startTime = maxDate.maxDate
                And musicianRehearsalEntity.id.musicianId >= 0
             ORDER BY musicianRehearsalEntity.rehearsal.date
            """)
    List<MusicianRehearsalEntity> findAllActivesMusiciansLastRehearsalUntilDateTime(LocalDateTime dateTime);

    @Query("""
             SELECT musicianRehearsalEntity
             FROM MusicianRehearsalEntity musicianRehearsalEntity
             WHERE musicianRehearsalEntity.deleteDateMR Is Null
                And musicianRehearsalEntity.id.rehearsalId = :rehearsalId
                And musicianRehearsalEntity.id.musicianId >= 0
            """)
    List<MusicianRehearsalEntity> findAllActivesMusiciansByRehearsalId(Long rehearsalId);

    @Query("""
             SELECT musicianRehearsalEntity.musician.id
             FROM MusicianRehearsalEntity musicianRehearsalEntity
             WHERE musicianRehearsalEntity.deleteDateMR Is Null
                And musicianRehearsalEntity.id.rehearsalId = :rehearsalId
                And musicianRehearsalEntity.id.musicianId >= 0
            """)
    List<Long> findAllActivesMusiciansIdByRehearsalId(Long rehearsalId);

    @Query("""
             SELECT musicianRehearsalEntity.id.rehearsalId as rehearsalId,
                    musicianRehearsalEntity.id.musicianId as musicianId,
                    musicianRehearsalEntity.formationPositionX as formationPositionX,
                    musicianRehearsalEntity.formationPositionY as formationPositionY,
                    musicianRehearsalEntity.rehearsal as rehearsal
             FROM MusicianRehearsalEntity musicianRehearsalEntity
             WHERE musicianRehearsalEntity.deleteDateMR Is Null
                And musicianRehearsalEntity.id.rehearsalId = :rehearsalId
                And musicianRehearsalEntity.id.musicianId < 0
            """)
    List<MusicianRehearsalProjection> findAllActivesFakeMusiciansByRehearsalId(Long rehearsalId);

    @Modifying
    @Query("""
            DELETE FROM MusicianRehearsalEntity musicianRehearsalEntity
            WHERE musicianRehearsalEntity.id.musicianId <0
                And musicianRehearsalEntity.id.rehearsalId = :rehearsalId  
            """)
    void deleteFakeMusicians(Long rehearsalId);

}
