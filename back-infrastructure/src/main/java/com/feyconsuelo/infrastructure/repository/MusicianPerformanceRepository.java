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

    @Query("""
             SELECT musicianPerformanceEntity
             FROM MusicianPerformanceEntity musicianPerformanceEntity
             WHERE musicianPerformanceEntity.deleteDateMP Is Null
                And musicianPerformanceEntity.id.performanceId = :performanceId
                And musicianPerformanceEntity.id.musicianId >= 0
            """)
    List<MusicianPerformanceEntity> findAllActivesMusiciansByPerformanceId(Long performanceId);

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
}
