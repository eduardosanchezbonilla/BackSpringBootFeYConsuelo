package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalPK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface RepertoireRehearsalRepository extends JpaRepository<RepertoireRehearsalEntity, RepertoireRehearsalPK> {

    @Query("""
             SELECT repertoireRehearsalEntity
             FROM RepertoireRehearsalEntity repertoireRehearsalEntity
             WHERE repertoireRehearsalEntity.deleteDateRR Is Null
                And repertoireRehearsalEntity.id.rehearsalId = :rehearsalId
            """)
    List<RepertoireRehearsalEntity> findAllActivesRepertoireMarchsByRehearsalId(Long rehearsalId);
}
