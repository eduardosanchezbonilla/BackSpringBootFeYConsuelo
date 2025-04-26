package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalMusiciansProjection;
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


    @Query(value = """
             SELECT r.id as rehearsalId,
                   r.date as date,
                   r.start_time as startTime,
                   r.end_time as endTime,
                   r.description as description,
                   r.voice_id_list as voiceIdList,
                   r.location as location,
                   r.municipality as municipality,
                   r.province as province,
                   r.duration as duration,
                   (
                       SELECT json_agg(
                                 json_build_object(
                                   'id',m.id,
                                   'dni',m.dni,
                                   'name',Trim(m.name),
                                   'surname',Trim(m.surname),
                                   'direction',Trim(m.direction),
                                   'municipality',Trim(m.municipality),
                                   'province',Trim(m.province),
                                   'email',Trim(m.email),
                                   'voice',json_build_object(
                                            'id',v.id,
                                            'name',v.name,
                                            'order',v.voice_order
                                        ),
                                   'image',m.image_thumbnail,
                                   'deleteDate',m.delete_date,
                                   'birthDate',m.birth_Date,
                                   'registrationDate',m.registration_Date,
                                   'unregistrationDate',m.unregistration_Date,
                                   'dateLastNotificationNonAssistsStreakRehearsals',m.date_Last_Notification_Non_Assists_Streak_Rehearsals,
                                   'inventoryObservations',m.inventory_Observations,
                                   'phoneNumber',m.phoneNumber,
                                   'unregistred',m.unregistred,
                                   'observations',m.observations,
                                   'assistLastRehearsal', (
                                                        CASE
                                                           WHEN mr.musician_id IS NOT NULL THEN true
                                                           ELSE false
                                                        END
                                                       ),
                                   'assistBus', false,
                                   'dateLastRehearsal',r.date,
                                   'idLastRehearsal',r.id,
                                   'formationPositionX',mr.formation_x_Position,
                                   'formationPositionY',mr.formation_y_Position
                                 )
                               )
                       FROM feyconsuelo.voice AS v,
                         feyconsuelo.musician AS m
                       Left Join feyconsuelo.musician_rehearsal mr
                        On (
                            r.id = mr.rehearsal_id and
                            mr.musician_id = m.id and
                            mr.delete_date is null and
                            mr.musician_id >=0
                           )
                       WHERE m.voice_id = v.id
                          and m.delete_date is null
                       And (m.unregistration_date Is Null Or m.unregistration_date > r.date)
                       And CAST(m.registration_date AS DATE) <= CAST(r.date AS DATE)
                      ) AS musicians,
                      (
                       SELECT json_agg(
                                 json_build_object(
                                   'id',mr.musician_id,
                                   'dni','',
                                   'name','Hueco',
                                   'surname','',
                                   'direction','',
                                   'municipality','',
                                   'province','',
                                   'email','',
                                   'voice',json_build_object(
                                            'id','-1',
                                            'name','HUECO',
                                            'order','0'
                                        ),
                                   'image','',
                                   'deleteDate',null,
                                   'birthDate',null,
                                   'registrationDate',null,
                                   'unregistrationDate',null,
                                   'dateLastNotificationNonAssistsStreakRehearsals',null,
                                   'inventoryObservations',null,
                                   'phoneNumber',null,
                                   'unregistred',null,
                                   'observations',null,
                                   'assistLastRehearsal', true,
                                   'assistBus', false,
                                   'dateLastRehearsal',r.date,
                                   'idLastRehearsal',r.id,
                                   'formationPositionX',mr.formation_x_Position,
                                   'formationPositionY',mr.formation_y_Position
                                 )
                               )
                       FROM feyconsuelo.musician_rehearsal mr       
                       WHERE mr.delete_date is null
                       and mr.musician_id <0
                          and mr.rehearsal_Id = :rehearsalId
                      ) AS fakeMusicians
               FROM feyconsuelo.rehearsal r
               WHERE r.delete_Date Is Null
                  And r.id = :rehearsalId                                                     
            """,
            nativeQuery = true)
    Optional<RehearsalMusiciansProjection> findRehearsalMusicians(Long rehearsalId);
}
