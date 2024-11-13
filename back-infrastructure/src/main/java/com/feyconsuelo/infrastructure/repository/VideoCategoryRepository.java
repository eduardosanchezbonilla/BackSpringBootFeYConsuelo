package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.videocategory.VideoCategoryEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface VideoCategoryRepository extends JpaRepository<VideoCategoryEntity, Long> {

    @Query("""
             SELECT videoCategory
             FROM VideoCategoryEntity videoCategory
             WHERE videoCategory.deleteDate Is Null
             ORDER BY videoCategory.id
            """)
    List<VideoCategoryEntity> findAllActives();

    @Query("""
             SELECT videoCategory
             FROM VideoCategoryEntity videoCategory
             WHERE videoCategory.deleteDate Is Null
               And videoCategory.id = :videoCategoryId
            """)
    Optional<VideoCategoryEntity> findVideoCategoryActiveById(Long videoCategoryId);

}
