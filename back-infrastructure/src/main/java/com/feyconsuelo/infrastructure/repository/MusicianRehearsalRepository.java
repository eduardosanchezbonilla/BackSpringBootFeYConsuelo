package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalPK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;

public interface MusicianRehearsalRepository extends JpaRepository<MusicianRehearsalEntity, MusicianRehearsalPK> {

    @Query("""
             SELECT musicianRehearsalEntity
             FROM MusicianRehearsalEntity musicianRehearsalEntity
             WHERE musicianRehearsalEntity.deleteDateMR Is Null
                And musicianRehearsalEntity.musician.id = :musicianId
                And musicianRehearsalEntity.rehearsal.date >= :startDate
                And musicianRehearsalEntity.rehearsal.date <= :endDate
             ORDER BY musicianRehearsalEntity.rehearsal.date
            """)
    List<MusicianRehearsalEntity> findAllActives(Long musicianId, LocalDate startDate, LocalDate endDate);


}
