package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoiremarchtype.RepertoireMarchTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface RepertoireMarchTypeRepository extends JpaRepository<RepertoireMarchTypeEntity, Long> {

    @Query("""
             SELECT repertoireMarchType
             FROM RepertoireMarchTypeEntity repertoireMarchType
             WHERE repertoireMarchType.repertoireMarchTypeDeleteDate Is Null
             ORDER BY repertoireMarchType.id
            """)
    List<RepertoireMarchTypeEntity> findAllActives();

    @Query("""
             SELECT repertoireMarchType
             FROM RepertoireMarchTypeEntity repertoireMarchType
             WHERE repertoireMarchType.repertoireMarchTypeDeleteDate Is Null
               And repertoireMarchType.id = :repertoireMarchTypeId
            """)
    Optional<RepertoireMarchTypeEntity> findRepertoireMarchTypeActiveById(Long repertoireMarchTypeId);

}
