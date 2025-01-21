package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.musicianrehearsal.MusicianRehearsalPK;
import com.feyconsuelo.infrastructure.entities.statistics.EventAssistStatistics;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;

public interface StatisticsAssistEventRepository extends JpaRepository<MusicianRehearsalEntity, MusicianRehearsalPK> {

    @Query(value = """
               With
               assist_rehearsal_day as (
                    Select r.date As date, Count(mr.musician_id) As assitsNumber
                    From feyconsuelo.rehearsal r
                    Left Join feyconsuelo.musician_rehearsal mr
                        On r.id = mr.rehearsal_id	 	
                    Where mr.delete_date Is null
                      And r.delete_date Is Null
                      And r.date >= :startDate
                          And r.date <= :endDate
                          And r.date < CURRENT_DATE
                    Group By r.date
               )
               Select
                   Round(Avg(ard.assitsNumber),2) AS averageAssitsNumber,
                   Max(ard.assitsNumber) AS maxAssitsNumber,
                   (
                     Select ard_inner.date
                     From assist_rehearsal_day ard_inner
                     Order By ard_inner.assitsNumber Desc
                     Limit 1
                   ) As dateMaxAssitsNumber,
                   Min(ard.assitsNumber) As minAssitsNumber,
                   (
                     Select ard_inner.date
                     From assist_rehearsal_day ard_inner
                     Order By ard_inner.assitsNumber Asc
                     Limit 1
                   ) As dateMinAssitsNumber
               From
                   assist_rehearsal_day ard;
            """, nativeQuery = true)
    EventAssistStatistics getEventsAssistStatistics(LocalDate startDate,
                                                    LocalDate endDate);

}
