package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.entities.statistics.RepertoireMarchEventStatistics;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;

public interface StatisticsRepertoireMarchEventRepository extends JpaRepository<RepertoireMarchEntity, Long> {

    @Query(value = """
                Select rc.id as categoryId,
                       rc.name as categoryName,
                       rmt.id as typeId,
                       rmt.name as typeName,
                       rmt.image as typeImage,
                       rmt.repertoire_march_type_order as typeOrder,
                       marchs.id as marchId,
                       marchs.name as marchName,
                       marchs.author as marchAuthor,
                       marchs.total_numbers as quantity,
                       ROUND(
                          CASE
                            WHEN max_total_numbers = 0 THEN 0
                            ELSE (CAST(total_numbers AS NUMERIC) / max_total_numbers) * 100
                          END,
                          2
                       ) as percentage,
                       marchs.max_total_numbers as maxQuantity
                from feyconsuelo.repertoire_category rc,
                     feyconsuelo.repertoire_march_type rmt,
                     (	
                        select COALESCE(stats.numbers, 0) AS total_numbers,
                               MAX(COALESCE(stats.numbers, 0)) OVER () AS max_total_numbers,
                               rm.*
                        from feyconsuelo.repertoire_march rm
                             Left Join (
                                        select stats.march_id,
                                               sum(stats.numbers) as numbers
                                        From(
                                             SELECT rmr.march_id as march_id,
                                                     Sum(rmr.march_numbers) numbers
                                              FROM feyconsuelo.rehearsal r,
                                                   feyconsuelo.repertoire_march_rehearsal rmr
                                              WHERE r.id = rmr.rehearsal_id
                                                And rmr.delete_date is Null
                                                And r.delete_date Is Null
                                                And r.date >= :startDate
                                                And r.date <= :endDate
                                                And DATE(r.date) <= DATE(CURRENT_DATE)
                                              group by rmr.march_id
                                             union all
                                             SELECT rmp.march_id as march_id,
                                                     Sum(rmp.march_numbers) numbers
                                              FROM feyconsuelo.performance p,
                                                   feyconsuelo.repertoire_march_performance rmp
                                              WHERE p.id = rmp.performance_id
                                                And rmp.delete_date is Null
                                                And p.delete_date Is Null
                                                And p.date >= :startDate
                                                And p.date <= :endDate
                                                And DATE(p.date) <= DATE(CURRENT_DATE)
                                              group by rmp.march_id
                                            ) stats
                                         group by march_id
                                        ) stats
                              On stats.march_id = rm.id
                        where rm.delete_date is null
                     ) marchs
                Where rc.id= marchs.category_id
                  And rmt.id = marchs.type_id
                  And rc."current" is true
                  And (
                        Exists( Select 1
                                From feyconsuelo.rehearsal r
                                Where r.delete_date Is Null
                                  And r.date >= :startDate
                                  And r.date <= :endDate
                                  And DATE(r.date) <= DATE(CURRENT_DATE)
                             )
                       or
                        Exists( Select 1
                                From feyconsuelo.performance p
                                Where p.delete_date Is Null
                                  And p.date >= :startDate
                                  And p.date <= :endDate
                                  And DATE(p.date) <= DATE(CURRENT_DATE)
                             )
                  )
            """,
            nativeQuery = true
    )
    List<RepertoireMarchEventStatistics> getRepertoireMarchEventStatistics(LocalDate startDate,
                                                                           LocalDate endDate);

}
