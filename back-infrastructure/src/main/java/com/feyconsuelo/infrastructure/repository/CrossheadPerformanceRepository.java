package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.performance.CrossheadPerformanceEntity;
import com.feyconsuelo.infrastructure.entities.performance.CrossheadProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface CrossheadPerformanceRepository extends JpaRepository<CrossheadPerformanceEntity, Long> {

    @Query(value = """
            
              SELECT
                  cp.id as crossheadPerformanceId,
                  cp.street as street,
                  cp.street_order as streetOrder,
                  cp.annotations as annotations,
                  cmp.id as crossheadMarchPerformanceId,
                  cmp.march_id as marchId,
                  cmp.march_name as marchName,
                  cmp.march_order as marchOrder,
                  cmp.annotations as marchAnnotations
              FROM
                  feyconsuelo.crosshead_performance AS cp
              LEFT OUTER JOIN
                  feyconsuelo.crosshead_march_performance AS cmp
                ON cp.id = cmp.crosshead_id
                   AND cmp.delete_date IS NULL
              WHERE
                  cp.performance_id = :performanceId
                  AND cp.delete_date IS NULL
              ORDER BY
                  cp.street_order,
                  cmp.march_order
            """,
            nativeQuery = true)
    List<CrossheadProjection> findCrossheadByPerformanceId(Long performanceId);

    @Modifying
    @Query("""
             DELETE
             FROM CrossheadPerformanceEntity crossheadPerformanceEntity
             WHERE crossheadPerformanceEntity.performanceId = :performanceId
            """)
    void deleteCrossheadByPerformanceId(Long performanceId);

}
