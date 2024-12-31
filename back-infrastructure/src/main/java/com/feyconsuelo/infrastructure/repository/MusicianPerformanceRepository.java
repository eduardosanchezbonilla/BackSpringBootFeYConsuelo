package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.musicianperformance.MusicianPerformancePK;
import org.springframework.data.jpa.repository.JpaRepository;
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
            """)
    List<MusicianPerformanceEntity> findAllActivesMusiciansByPerformanceId(Long performanceId);
}
