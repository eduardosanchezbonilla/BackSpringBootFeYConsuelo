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
                    select date,
                           voiceIdList,
                           assitsNumber,
                           maximumAssistsNumber,
                           case
                             when maximumAssistsNumber = 0 then 100
                             else ROUND((CAST(assitsNumber AS NUMERIC) / maximumAssistsNumber) * 100, 2)
                           end as assitsPercentage
                    From(
                        Select r.date As date,
                               r.voice_id_list as voiceIdList,
                               --Count(mr.musician_id) As assitsNumber,
                               (
                                Select count(mr.musician_id)
                                From feyconsuelo.musician_rehearsal mr\s
                                Where r.id = mr.rehearsal_id
                                  and mr.delete_date Is null
                               ) as assitsNumber,
                               (
                                Select count(1)
                                From feyconsuelo.musician m
                                Where m.voice_id = Any(r.voice_id_list)
                                  And m.delete_Date is null
                                  And DATE(r.date) >= DATE(m.registration_date)
                                  And (m.unregistred is false Or (m.unregistred is true And DATE(r.date) < DATE(m.unregistration_Date)) )
                               ) as maximumAssistsNumber
                        From feyconsuelo.rehearsal r
                        --Left Join feyconsuelo.musician_rehearsal mr
                        --    On r.id = mr.rehearsal_id
                        --Where mr.delete_date Is null
                        Where r.delete_date Is Null
                          And r.date >= :startDate
                          And r.date <= :endDate
                          And r.date < CURRENT_DATE
                        --Group By r.date, r.voice_id_list
                        Group By r.id, r.date, r.voice_id_list
                    ) as days
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
                   ) As dateMinAssitsNumber,
                   Round(Avg(ard.assitsPercentage),2) AS averageAssitsPercentage,
                   Max(ard.assitsPercentage) AS maxAssitsPercentage,
                   (
                     Select ard_inner.date
                     From assist_rehearsal_day ard_inner
                     Order By ard_inner.assitsPercentage Desc
                     Limit 1
                   ) As dateMaxAssitsPErcentage,
                   Min(ard.assitsPercentage) As minAssitsPercentage,
                   (
                     Select ard_inner.date
                     From assist_rehearsal_day ard_inner
                     Order By ard_inner.assitsPercentage Asc
                     Limit 1
                   ) As dateMinAssitsPercentage
               From
                   assist_rehearsal_day ard;
            """, nativeQuery = true)
    EventAssistStatistics getEventsAssistStatistics(LocalDate startDate,
                                                    LocalDate endDate);

}
