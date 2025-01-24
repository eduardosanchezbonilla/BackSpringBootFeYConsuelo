package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.entities.statistics.MusicianEventAssistStatistics;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDate;
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

    @Query("""
             SELECT musicianRequest
             FROM MusicianEntity musicianRequest
             WHERE musicianRequest.deleteDate Is Null
               And musicianRequest.voice.id = :voiceId
            """)
    List<MusicianEntity> findMusicianActiveByVoice(Long voiceId);

    @Query("""
                SELECT m
                FROM MusicianEntity m
                WHERE m.deleteDate IS NULL
                  AND MONTH(m.birthDate) = :month
                  AND DAY(m.birthDate) = :day
            """)
    List<MusicianEntity> findMusicianActiveByBirthdayDate(@Param("month") int month,
                                                          @Param("day") int day);

    @Query(value = """
             With
                  all_events as (
                    Select r.id,r.date,r.voice_id_list,r.description, 'REHEARSAL' as Type
                    From feyconsuelo.rehearsal r,
                         feyconsuelo.musician m
                    Where r.delete_date Is null
                      And m.id = :musicianId
                      And r.date >= m.registration_date
                      And r.date <= CURRENT_DATE
                      And m.voice_id = Any(r.voice_id_list)
                    Union
                    Select p.id,p.date,p.voice_id_list,p.description, 'PERFORMANCE' as Type
                    From feyconsuelo.performance p,
                         feyconsuelo.musician m
                    Where p.delete_date Is null
                      And m.id = :musicianId
                      And p.date >= m.registration_date
                      And p.date <= CURRENT_DATE
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
                      And r.date >= m.registration_date
                      And r.date <= CURRENT_DATE
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
                      And p.date >= m.registration_date
                      And p.date <= CURRENT_DATE
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

}
