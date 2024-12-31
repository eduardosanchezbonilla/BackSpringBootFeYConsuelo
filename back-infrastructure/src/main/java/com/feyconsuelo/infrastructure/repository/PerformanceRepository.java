package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
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
    List<PerformanceEntity> findAllActives(LocalDate startDate,
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

}
