package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformanceEntity;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformancePK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface RepertoirePerformanceRepository extends JpaRepository<RepertoirePerformanceEntity, RepertoirePerformancePK> {

    @Query("""
             SELECT repertoirePerformanceEntity
             FROM RepertoirePerformanceEntity repertoirePerformanceEntity
             WHERE repertoirePerformanceEntity.deleteDateRP Is Null
                And repertoirePerformanceEntity.id.performanceId = :performanceId
            """)
    List<RepertoirePerformanceEntity> findAllActivesRepertoireMarchsByPerformanceId(Long performanceId);
}
