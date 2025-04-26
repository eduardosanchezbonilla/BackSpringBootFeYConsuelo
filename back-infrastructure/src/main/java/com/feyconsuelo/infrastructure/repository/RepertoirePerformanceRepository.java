package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformanceEntity;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformancePK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface RepertoirePerformanceRepository extends JpaRepository<RepertoirePerformanceEntity, RepertoirePerformancePK> {

    @Query("""
             SELECT repertoirePerformanceEntity
             FROM RepertoirePerformanceEntity repertoirePerformanceEntity
             WHERE repertoirePerformanceEntity.deleteDateRP Is Null
                And repertoirePerformanceEntity.id.performanceId = :performanceId
            """)
    List<RepertoirePerformanceEntity> findAllActivesRepertoireMarchsByPerformanceId(Long performanceId);

    @Query(value = """
            SELECT json_build_object(
                                  'event',json_build_object(
                                              'id',p.id,
                                              'date',p.date,
                                              'type','PERFORMANCE',
                                              'clsClass','ACTUACION_DAY',
                                              'startTime',p.start_time,
                                              'endTime',p.end_time,
                                              'title',p.title,
                                              'description',p.description,
                                              'performanceType',p.performance_type,
                                              'location',p.location,
                                              'municipality',p.municipality,
                                              'province',p.province, 
                                              'image',p.image_thumbnail,
                                              'displacementBus',p.bus,                      
                                              'eventPublic',p.event_public,
                                              'repertoirePublic',p.repertoire_public,
                                              'crossheadPublic',p.crosshead_public,
                                              'busData',p.bus_data,
                                              'busTime',p.bus_time,
                                              'busData',p.bus_data,
                                              'busLocation',p.bus_location,
                                              'duration',p.duration,	                  
                                              'kilometers',p.kilometers,
                                              'googleId',p.google_Id
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
                                                                                                 WHEN rmp.march_id IS NOT NULL THEN true
                                                                                                 ELSE false
                                                                                              END
                                                                                             ),
                                                                                  'order',rmp.march_order,
                                                                                  'numbers',(
                                                                                              CASE
                                                                                                 WHEN rmp.march_numbers IS NOT NULL THEN rmp.march_numbers
                                                                                                 ELSE 0
                                                                                              END
                                                                                             )		                  
                                                                                )
                                                                              )
                                                                      FROM feyconsuelo.repertoire_category AS rc,
                                                                           feyconsuelo.repertoire_march AS rm
                                                                      Left Join feyconsuelo.repertoire_march_performance rmp
                                                                          On (
                                                                              p.id = rmp.performance_id and
                                                                              rmp.march_id = rm.id and
                                                                              rmp.delete_date is null
                                                                             )
                                                                      WHERE rm.category_id = rc.id
                                                                         and rm.type_id = rmt.id
                                                                         and rm.delete_date is null	
                                                                         and (rc.current Is true or rmp.march_id IS NOT NULL)
                                                                     )		                 
                                                            )
                                                      )
                                              FROM feyconsuelo.repertoire_march_type rmt        	
                                              WHERE rmt.delete_date is null
                                   )                              	            		
                             )	
                        FROM feyconsuelo.performance p
                        WHERE p.delete_Date Is Null
                           And p.id = :performanceId
            """,
            nativeQuery = true)
    Optional<String> findEventRepertoireRehearsalProjection(Long performanceId);
}
