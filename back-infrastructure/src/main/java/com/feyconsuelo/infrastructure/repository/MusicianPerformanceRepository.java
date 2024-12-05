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
                And musicianPerformanceEntity.performance.date >= :startDate
                And musicianPerformanceEntity.performance.date <= :endDate
             ORDER BY musicianPerformanceEntity.performance.date
            """)
    List<MusicianPerformanceEntity> findAllActives(Long musicianId, LocalDate startDate, LocalDate endDate);


}
