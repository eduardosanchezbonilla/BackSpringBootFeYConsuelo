package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface MusicianRepository extends JpaRepository<MusicianEntity, Long> {

    @Query("""
             SELECT musicianRequest
             FROM MusicianEntity musicianRequest
             WHERE musicianRequest.deleteDate Is Null
             ORDER BY musicianRequest.id
            """)
    List<MusicianEntity> findAllActives();

    @Query("""
             SELECT musicianRequest
             FROM MusicianEntity musicianRequest
             WHERE musicianRequest.deleteDate Is Null
               And musicianRequest.id = :musicianId
            """)
    Optional<MusicianEntity> findMusicianActiveById(Long musicianId);

    @Query("""
             SELECT musicianRequest
             FROM MusicianEntity musicianRequest
             WHERE musicianRequest.deleteDate Is Null
               And musicianRequest.dni = :dni
            """)
    Optional<MusicianEntity> findMusicianActiveByDni(String dni);
}
