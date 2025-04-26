package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalPK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface RepertoireRehearsalRepository extends JpaRepository<RepertoireRehearsalEntity, RepertoireRehearsalPK> {

    @Query("""
             SELECT repertoireRehearsalEntity
             FROM RepertoireRehearsalEntity repertoireRehearsalEntity
             WHERE repertoireRehearsalEntity.deleteDateRR Is Null
                And repertoireRehearsalEntity.id.rehearsalId = :rehearsalId
            """)
    List<RepertoireRehearsalEntity> findAllActivesRepertoireMarchsByRehearsalId(Long rehearsalId);


    @Query(value = """
            SELECT json_build_object(
                                   'event',json_build_object(
                                               'id',r.id,
                                               'date',r.date,
                                               'type','REHEARSAL',
                                               'clsClass','ENSAYO_GENERAL_DAY',
                                               'startTime',r.start_time,
                                               'endTime',r.end_time,
                                               'location',r.location,
                                               'municipality',r.municipality,
                                               'province',r.province,
                                               'duration',r.duration	                   
                                           ),                       	
                                   'marchs', (
                                               SELECT json_agg(
                                                         json_build_object(
                                                           'type',json_build_object(
                                                                       'id',rmt.id,
                                                                       'name',rmt.name,
                                                                       'image',rmt.image,
                                                                       'order',rmt.repertoire_march_type_order
                                                                   ),
                                                           'marchs',(
                                                                       SELECT json_agg(
                                                                                 json_build_object(
                                                                                   'id',rm.id,
                                                                                   'categoryId',rm.category_id,
                                                                                   'category',json_build_object(
                                                                                       'id',rc.id,
                                                                                       'name',rc.name,
                                                                                       --'image',rmt.image,
                                                                                       'order',rc.repertoire_category_order,
                                                                                       'current', current
                                                                                   ),
                                                                                   'name',Trim(rm.name),
                                                                                   'author',Trim(rm.author),
                                                                                   'description',Trim(rm.description),
                                                                                   'youtubeId',Trim(rm.youtube_Id),
                                                                                   'checked',(
                                                                                               CASE
                                                                                                  WHEN rmr.march_id IS NOT NULL THEN true
                                                                                                  ELSE false
                                                                                               END
                                                                                              ),
                                                                                   'order',rmr.march_order,
                                                                                   'numbers',(
                                                                                               CASE
                                                                                                  WHEN rmr.march_numbers IS NOT NULL THEN rmr.march_numbers
                                                                                                  ELSE 0
                                                                                               END
                                                                                              )					                   
                                                                                 )
                                                                               )
                                                                       FROM feyconsuelo.repertoire_category AS rc,
                                                                            feyconsuelo.repertoire_march AS rm
                                                                       Left Join feyconsuelo.repertoire_march_rehearsal rmr
                                                                           On (
                                                                               r.id = rmr.rehearsal_id and
                                                                               rmr.march_id = rm.id and
                                                                               rmr.delete_date is null
                                                                              )
                                                                       WHERE rm.category_id = rc.id
                                                                          and rm.type_id = rmt.id
                                                                          and rm.delete_date is null	
                                                                          and (rc.current Is true or rmr.march_id IS NOT NULL)
                                                                      )		                   
                                                             )
                                                       )
                                               FROM feyconsuelo.repertoire_march_type rmt        	
                                               WHERE rmt.delete_date is null
                                              )                                	            		
                              )	
                         FROM feyconsuelo.rehearsal r
                         WHERE r.delete_Date Is Null
                            And r.id = :rehearsalId
            """,
            nativeQuery = true)
    Optional<String> findEventRepertoireRehearsalProjection(Long rehearsalId);
}
