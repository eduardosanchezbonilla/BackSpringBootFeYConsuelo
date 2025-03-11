package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.performance.CrossheadPerformanceEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface CrossheadPerformanceRepository extends JpaRepository<CrossheadPerformanceEntity, Long> {

    @Query("""
             SELECT crossheadPerformanceEntity
             FROM CrossheadPerformanceEntity crossheadPerformanceEntity
             WHERE crossheadPerformanceEntity.deleteDateCrosshead Is Null
               And crossheadPerformanceEntity.performanceId = :performanceId
            """)
    List<CrossheadPerformanceEntity> findCrossheadByPerformanceId(Long performanceId);

    @Modifying
    @Query("""
             DELETE
             FROM CrossheadPerformanceEntity crossheadPerformanceEntity
             WHERE crossheadPerformanceEntity.performanceId = :performanceId
            """)
    void deleteCrossheadByPerformanceId(Long performanceId);

}
