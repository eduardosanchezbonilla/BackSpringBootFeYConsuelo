package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoirecategory.RepertoireCategoryEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface RepertoireCategoryRepository extends JpaRepository<RepertoireCategoryEntity, Long> {

    @Query("""
             SELECT repertoireCategory
             FROM RepertoireCategoryEntity repertoireCategory
             WHERE repertoireCategory.repertoireDeleteDate Is Null
             ORDER BY repertoireCategory.id
            """)
    List<RepertoireCategoryEntity> findAllActives();

    @Query("""
             SELECT repertoireCategory
             FROM RepertoireCategoryEntity repertoireCategory
             WHERE repertoireCategory.repertoireDeleteDate Is Null
               And repertoireCategory.id = :repertoireCategoryId
            """)
    Optional<RepertoireCategoryEntity> findRepertoireCategoryActiveById(Long repertoireCategoryId);

}
