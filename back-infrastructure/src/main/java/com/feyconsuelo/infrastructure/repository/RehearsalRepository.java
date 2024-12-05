package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface RehearsalRepository extends JpaRepository<RehearsalEntity, Long> {

    @Query("""
             SELECT rehearsalEntity
             FROM RehearsalEntity rehearsalEntity
             WHERE rehearsalEntity.deleteDate Is Null
                And rehearsalEntity.date >= :startDate
                And rehearsalEntity.date <= :endDate
             ORDER BY rehearsalEntity.id
            """)
    List<RehearsalEntity> findAllActives(LocalDate startDate, LocalDate endDate);

    @Query("""
             SELECT rehearsalEntity
             FROM RehearsalEntity rehearsalEntity
             WHERE rehearsalEntity.deleteDate Is Null
               And rehearsalEntity.id = :rehearsalId
            """)
    Optional<RehearsalEntity> findRehearsalActiveById(Long rehearsalId);

    @Query("""
             SELECT rehearsalEntity
             FROM RehearsalEntity rehearsalEntity
             WHERE rehearsalEntity.deleteDate Is Null
                And rehearsalEntity.date = :date
            """)
    Optional<RehearsalEntity> findRehearsalActiveByDate(LocalDate date);

}
