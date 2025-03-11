package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.musician.MusicianEntity;
import com.feyconsuelo.infrastructure.entities.musiciansolostats.MusicianSoloStats;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface MusicianSoloStatsRepository extends JpaRepository<MusicianEntity, Long> {

    @Query(value = """
             with
             resumeSolos as (
                select m.id,
                       m.name,
                       m.surname,
                       (
                        select count(1)
                        from feyconsuelo.repertoire_march_main_soloist rmms
                        where rmms.musician_id = m.id
                       ) numeroSolosPrincipales,
                       (
                        select count(1)
                        from feyconsuelo.repertoire_march_secondary_soloist rmss
                        where rmss.musician_id = m.id
                       ) numeroSolosSuplente
                from feyconsuelo.musician m
                where m.delete_date is null
             )
             select rs.id as id,
                   rs.name as name,
                   rs.surname as surname,
                   (rs.numeroSolosPrincipales+rs.numeroSolosSuplente) as totalSolos,
                   rs.numeroSolosPrincipales as totalMainSolos,
                   rs.numeroSolosSuplente as totalSecondarySolos
             from resumeSolos rs
             where rs.numeroSolosPrincipales > 0
               or rs.numeroSolosSuplente > 0
             order by 3 desc,4 desc
            """, nativeQuery = true)
    List<MusicianSoloStats> getMusicianSoloStats();

}


