package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface RepertoireMarchRepository extends JpaRepository<RepertoireMarchEntity, Long> {

    @Query("""
             SELECT repertoireMarch
             FROM RepertoireMarchEntity repertoireMarch
             WHERE repertoireMarch.repertoireMarchDeleteDate Is Null
             ORDER BY repertoireMarch.id
            """)
    List<RepertoireMarchEntity> findAllActives();

    @Query(value = """
             select rm.id as id,
                   rm.category_id  as categoryId,
                   json_build_object(						
                        'id',rc.id,
                        'name',rc."name",
                        'order',rc.repertoire_category_order,
                        --'image',rc.image,
                        'deleteDate',rc.delete_date,
                        'current',rc.current
                   ) as category,
                   rm.type_id as typeId,
                   json_build_object(						
                        'id',rmt.id,
                        'name',rmt."name",
                        'order',rmt.repertoire_march_type_order,
                        --'image',rc.image,
                        'deleteDate',rc.delete_date
                   ) as type,
                   rm.name as name,
                   rm.author as author,
                   rm.description as description,
                   rm.youtube_id as youtubeId,
                   rm.delete_date as deleteDate,
                   (
                    SELECT json_agg(
                              json_build_object(
                                'id',       rms.id,
                                'order',    rms.solo_order,
                                'name',     rms.name,
                                'mainSoloists', (
                                                    SELECT json_agg(
                                                              json_build_object(
                                                                'musicianId', rmms.musician_id,
                                                                'musicianName', rmms.musician_name,
                                                                'order', rmms.soloist_order
                                                              )
                                                              ORDER BY rmms.soloist_order
                                                            )
                                                    FROM feyconsuelo.repertoire_march_main_soloist AS rmms	   
                                                    WHERE rmms.solo_id = rms.id
                                                  ),
                                'secondarySoloists', (
                                                    SELECT json_agg(
                                                              json_build_object(
                                                                'musicianId', rmss.musician_id,
                                                                'musicianName', rmss.musician_name,
                                                                'order', rmss.soloist_order
                                                              )
                                                              ORDER BY rmss.soloist_order
                                                            )
                                                    FROM feyconsuelo.repertoire_march_secondary_soloist AS rmss	   
                                                    WHERE rmss.solo_id = rms.id
                                                  )
                              )
                              ORDER BY rms.solo_order
                            )
                    FROM feyconsuelo.repertoire_march_solo AS rms	   
                    WHERE rms.march_id = rm.id
                  ) AS solos
            from feyconsuelo.repertoire_march rm,
                 feyconsuelo.repertoire_category rc,
                 feyconsuelo.repertoire_march_type rmt
            where rc.id = rm.category_id
              and rmt.id = rm.type_id
              and rm.delete_date is null
              and rm.category_id = :categoryId
            """,
            nativeQuery = true)
    List<RepertoireMarchProjection> findAllActivesByCategoryId(Long categoryId);

    @Query("""
             SELECT repertoireMarch
             FROM RepertoireMarchEntity repertoireMarch
             WHERE repertoireMarch.repertoireMarchDeleteDate Is Null
                And repertoireMarch.typeEntity.id = :typeId
             ORDER BY repertoireMarch.id
            """)
    List<RepertoireMarchEntity> findAllActivesByTypeId(Long typeId);

    @Query("""
             SELECT repertoireMarch
             FROM RepertoireMarchEntity repertoireMarch
             WHERE repertoireMarch.repertoireMarchDeleteDate Is Null
               And repertoireMarch.id = :repertoireMarchId
            """)
    Optional<RepertoireMarchEntity> findRepertoireMarchActiveById(Long repertoireMarchId);

}
