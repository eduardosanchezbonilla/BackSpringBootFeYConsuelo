package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.video.VideoEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface VideoRepository extends JpaRepository<VideoEntity, Long> {

    @Query("""
             SELECT videoEntity
             FROM VideoEntity videoEntity
             WHERE videoEntity.deleteDate Is Null
             ORDER BY videoEntity.id
            """)
    List<VideoEntity> findAllActives();

    @Query("""
             SELECT videoEntity
             FROM VideoEntity videoEntity
             WHERE videoEntity.deleteDate Is Null
               And videoEntity.id = :videoId
            """)
    Optional<VideoEntity> findVideoActiveById(Long videoId);

    @Query("""
             SELECT videoEntity
             FROM VideoEntity videoEntity
             WHERE videoEntity.deleteDate Is Null
               And videoEntity.videoCategory.id = :categoryId
            """)
    List<VideoEntity> findVideoActiveByCategory(Long categoryId);

}
