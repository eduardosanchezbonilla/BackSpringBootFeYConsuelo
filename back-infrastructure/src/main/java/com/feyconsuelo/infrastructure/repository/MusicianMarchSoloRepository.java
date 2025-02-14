package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musicianmarchsolo.MusicianMarchSolo;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface MusicianMarchSoloRepository extends JpaRepository<RepertoireMarchEntity, Long> {

    @Query(value = """
             with
               solos as (
                select rm.name as marchName,
                       rms.name as soloName,
                       rms.solo_order as soloOrder,
                       (
                        select rmms.soloist_order
                        from feyconsuelo.repertoire_march_main_soloist rmms
                        where rms.id = rmms.solo_id
                          and rmms.musician_id = :musicianId
                       ) mainSoloistOrder,
                       (
                        select min(rmms.soloist_order)
                        from feyconsuelo.repertoire_march_main_soloist rmms
                        where rms.id = rmms.solo_id
                       ) minMainOrder,
                       (
                        select max(rmms.soloist_order)
                        from feyconsuelo.repertoire_march_main_soloist rmms
                        where rms.id = rmms.solo_id	
                       ) maxMainOrder,
                       (
                        select rmss.soloist_order
                        from feyconsuelo.repertoire_march_secondary_soloist rmss
                        where rms.id = rmss.solo_id
                          and rmss.musician_id = :musicianId
                       ) secondarySoloistOrder,
                       (
                        select min(rmss.soloist_order)
                        from feyconsuelo.repertoire_march_secondary_soloist rmss
                        where rms.id = rmss.solo_id	
                       ) minSecondaryOrder,
                       (
                        select max(rmss.soloist_order)
                        from feyconsuelo.repertoire_march_secondary_soloist rmss
                        where rms.id = rmss.solo_id
                       ) maxSecondaryOrder
                from feyconsuelo.repertoire_march rm,
                     feyconsuelo.repertoire_march_solo rms
                where rm.id = rms.march_id
                order by rm.name, rms.solo_order
               )
               select *
               from solos s
               where s.mainsoloistorder is not null
                 or s.secondarysoloistorder is not null
               order by s.marchName, s.soloOrder
            """, nativeQuery = true)
    List<MusicianMarchSolo> findMusicianMarchSolos(Long musicianId);

}


