package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.entities.statistics.AllMusicianEventAssistStatistics;
import com.feyconsuelo.infrastructure.entities.statistics.MusicianEventAssistStatistics;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface StatisticsMusicianAssistEventRepository extends JpaRepository<MusicianEntity, Long> {

    @Query(value = """
             With
                  all_events as (
                    Select r.id,r.date,r.voice_id_list,r.description, 'REHEARSAL' as Type
                    From feyconsuelo.rehearsal r,
                         feyconsuelo.musician m
                    Where r.delete_date Is null
                      And m.id = :musicianId
                      And DATE(r.date) >= DATE(m.registration_date)
                      And DATE(r.date) <= DATE(CURRENT_DATE)
                      And m.voice_id = Any(r.voice_id_list)
                    Union
                    Select p.id,p.date,p.voice_id_list,p.description, 'PERFORMANCE' as Type
                    From feyconsuelo.performance p,
                         feyconsuelo.musician m
                    Where p.delete_date Is null
                      And m.id = :musicianId
                      And DATE(p.date) >= DATE(m.registration_date)
                      And DATE(p.date) <= DATE(CURRENT_DATE)
                      And m.voice_id = Any(p.voice_id_list)
                  ),
                  musician_events as (
                    Select mr.rehearsal_id, r.date, 'REHEARSAL'  as Type
                    From feyconsuelo.musician_rehearsal mr,
                         feyconsuelo.rehearsal r,
                         feyconsuelo.musician m
                    Where mr.delete_date Is null
                      And r.delete_date Is Null
                      And m.id = :musicianId
                      And m.id = mr.musician_id
                      And r.id = mr.rehearsal_id
                      And DATE(r.date) >= DATE(m.registration_date)
                      And DATE(r.date) <= DATE(CURRENT_DATE)
                      And m.voice_id = Any(r.voice_id_list)
                    Union
                    Select mp.performance_id,p.date, 'PERFORMANCE'  as Type
                    From feyconsuelo.musician_performance mp,
                         feyconsuelo.performance p,
                         feyconsuelo.musician m
                    Where mp.delete_date Is null
                      And p.delete_date Is Null
                      And m.id = :musicianId
                      And m.id = mp.musician_id
                      And p.id = mp.performance_id
                      And DATE(p.date) >= DATE(m.registration_date)
                      And DATE(p.date) <= DATE(CURRENT_DATE)
                      And m.voice_id = Any(p.voice_id_list)
                  )
                  Select (
                        Select count(1)
                        From all_events
                        Where type = 'REHEARSAL'
                       ) musicianHistoricTotalNumberRehearsalEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where type = 'REHEARSAL'
                       ) musicianHistoricAssistNumberRehearsalEvents,
                       (
                        Select count(1)
                        From all_events
                        Where type = 'PERFORMANCE'
                       ) musicianHistoricTotalNumberPerformanceEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where type = 'PERFORMANCE'
                       ) musicianHistoricAssistNumberPerformanceEvents,
                       (
                        Select count(1)
                        From all_events
                       ) musicianHistoricTotalNumberEvents,
                       (
                        Select count(1)
                        From musician_events
                       ) musicianHistoricAssistNumberEvents,
                       (
                        Select count(1)
                        From all_events
                        Where type = 'REHEARSAL'
                          And date >= :fromYearDate
                          And date <= :toYearDate
                       ) musicianFromDateTotalNumberRehearsalEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where type = 'REHEARSAL'
                            And date >= :fromYearDate
                            And date <= :toYearDate
                       ) musicianFromDateAssistNumberRehearsalEvents,
                       (
                        Select count(1)
                        From all_events
                        Where type = 'PERFORMANCE'
                            And date >= :fromYearDate
                            And date <= :toYearDate 		
                       ) musicianFromDateTotalNumberPerformanceEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where type = 'PERFORMANCE'
                            And date >= :fromYearDate
                            And date <= :toYearDate 		
                       ) musicianFromDateAssistNumberPerformanceEvents,
                       (
                        Select count(1)
                        From all_events
                        Where date >= :fromYearDate
                            And date <= :toYearDate 	
                       ) musicianFromDateTotalNumberEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where date >= :fromYearDate
                            And date <= :toYearDate
                       ) musicianFromDateAssistNumberEvents,
                       (
                        Select count(1)
                        From all_events
                        Where type = 'REHEARSAL'
                          And date >= :betweenDatesStart
                          And date <= :betweenDatesEnd
                       ) musicianBetweenDatesTotalNumberRehearsalEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where type = 'REHEARSAL'
                          And date >= :betweenDatesStart
                          And date <= :betweenDatesEnd
                       ) musicianBetweenDatesAssistNumberRehearsalEvents,
                       (
                        Select count(1)
                        From all_events
                        Where type = 'PERFORMANCE'
                          And date >= :betweenDatesStart
                          And date <= :betweenDatesEnd
                       ) musicianBetweenDatesTotalNumberPerformanceEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where type = 'PERFORMANCE'
                          And date >= :betweenDatesStart
                          And date <= :betweenDatesEnd		
                       ) musicianBetweenDatesAssistNumberPerformanceEvents,
                       (
                        Select count(1)
                        From all_events
                        Where date >= :betweenDatesStart
                          And date <= :betweenDatesEnd
                       ) musicianBetweenDatesTotalNumberEvents,
                       (
                        Select count(1)
                        From musician_events
                        Where date >= :betweenDatesStart
                          And date <= :betweenDatesEnd
                       ) musicianBetweenDatesAssistNumberEvents
            """, nativeQuery = true)
    Optional<MusicianEventAssistStatistics> getMusicianEventAssistStatistics(Long musicianId,
                                                                             LocalDate fromYearDate,
                                                                             LocalDate toYearDate,
                                                                             LocalDate betweenDatesStart,
                                                                             LocalDate betweenDatesEnd);

    @Query(value = """
                With
                all_events as (
                    Select r.id,r.date,r.voice_id_list
                    From feyconsuelo.rehearsal r
                    Where r.delete_date Is null	 
                      And r.date >= :startDate
                      And r.date <= :endDate
                      And r.date < CURRENT_DATE	 
                ),
                musician_events as (
                    Select mr.rehearsal_id, r.date, r.voice_id_list, mr.musician_id
                    From feyconsuelo.musician_rehearsal mr,
                         feyconsuelo.rehearsal r,
                         feyconsuelo.musician m
                    Where mr.delete_date Is null
                      And r.delete_date Is Null                      
                      And m.id = mr.musician_id
                      And r.id = mr.rehearsal_id
                      And r.date >= :startDate
                      And r.date <= :endDate
                      And r.date < CURRENT_DATE                      	
                ),
                musician_stats as(
                    select m.id,
                           m.name,
                           m.surname,
                           m.voice_id,
                           (
                            Select Count(1)
                            From all_events ae
                            where m.voice_id = Any(ae.voice_id_list)	
                              And DATE(ae.date) >= DATE(m.registration_date)
                           ) totalRehearsal,
                           (
                            Select Count(1)
                            From musician_events me
                            where m.voice_id = Any(me.voice_id_list)	
                              And DATE(me.date) >= DATE(m.registration_date)
                              and m.id = me.musician_id
                           ) musicianAssistsRehearsal
                    from feyconsuelo.musician m
                    where m.delete_date is  null
                )
                select m.id as musicianId,
                       m.name as musicianName,
                       m.surname as musicianSurname,
                       m.totalRehearsal as totalRehearsal,
                       m.musicianAssistsRehearsal as musicianAssistsRehearsal,	
                       ROUND((CAST(m.musicianAssistsRehearsal AS NUMERIC) / m.totalRehearsal) * 100, 2) AS musicianPercentageAssistsRehearsal 
                from musician_stats m,
                     feyconsuelo.voice v
                where m.voice_id = v.id
                  and m.totalRehearsal>0
                order by 5 asc,v.voice_order, m.id
            """, nativeQuery = true)
    List<AllMusicianEventAssistStatistics> getAllMusicianEventAssistStatistics(LocalDate startDate,
                                                                               LocalDate endDate);

}
