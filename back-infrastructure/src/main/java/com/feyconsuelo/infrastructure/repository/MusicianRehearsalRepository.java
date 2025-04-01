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
