package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

public interface RehearsalRepository extends JpaRepository<RehearsalEntity, Long> {

    @Query("""
             SELECT rehearsalEntity
             FROM RehearsalEntity rehearsalEntity
             WHERE rehearsalEntity.deleteDate Is Null
                And (rehearsalEntity.date >= :startDate Or :allStartDate = true)
                And (rehearsalEntity.date <= :endDate Or :allEndDate = true)
             ORDER BY rehearsalEntity.id
            """)
    List<RehearsalEntity> findAllActives(LocalDate startDate,
                                         LocalDate endDate,
                                         Boolean allStartDate,
                                         Boolean allEndDate
    );

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

    @Query("""
             SELECT rehearsalEntity2
             FROM RehearsalEntity rehearsalEntity2,
                  (
                   Select Max(rehearsalEntity.startTime) as maxDate
                   From RehearsalEntity rehearsalEntity
                   Where rehearsalEntity.deleteDate Is Null
                      And rehearsalEntity.startTime <= :dateTime
                  ) as maxDate
             WHERE rehearsalEntity2.deleteDate Is Null
                And rehearsalEntity2.startTime = maxDate.maxDate
            """)
    Optional<RehearsalEntity> findLastRehearsalUntilDateTime(LocalDateTime dateTime);

}
