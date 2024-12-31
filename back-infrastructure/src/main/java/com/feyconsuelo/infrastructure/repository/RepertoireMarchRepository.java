package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface RepertoireMarchRepository extends JpaRepository<RepertoireMarchEntity, Long> {

    @Query("""
             SELECT repertoireMarch
             FROM RepertoireMarchEntity repertoireMarch
             WHERE repertoireMarch.repertoireMarchDeleteDate Is Null
             ORDER BY repertoireMarch.id
            """)
    List<RepertoireMarchEntity> findAllActives();

    @Query("""
             SELECT repertoireMarch
             FROM RepertoireMarchEntity repertoireMarch
             WHERE repertoireMarch.repertoireMarchDeleteDate Is Null
                And repertoireMarch.categoryEntity.id = :categoryId
             ORDER BY repertoireMarch.id
            """)
    List<RepertoireMarchEntity> findAllActivesByCategoryId(Long categoryId);

    @Query("""
             SELECT repertoireMarch
             FROM RepertoireMarchEntity repertoireMarch
             WHERE repertoireMarch.repertoireMarchDeleteDate Is Null
                And repertoireMarch.typeEntity.id = :typeId
             ORDER BY repertoireMarch.id
            """)
    List<RepertoireMarchEntity> findAllActivesByTypeId(Long typeId);

    @Query("""
             SELECT repertoireMarch
             FROM RepertoireMarchEntity repertoireMarch
             WHERE repertoireMarch.repertoireMarchDeleteDate Is Null
               And repertoireMarch.id = :repertoireMarchId
            """)
    Optional<RepertoireMarchEntity> findRepertoireMarchActiveById(Long repertoireMarchId);

}
