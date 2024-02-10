package com.feyconsuelo.infrastructure.jpa.repository;

import com.feyconsuelo.domain.entity.musician.FindMusiciansRequest;
import com.feyconsuelo.infrastructure.jpa.entities.MusicianEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface MusicianJpaRepository extends JpaRepository<MusicianEntity, String> {
    
    @Query("SELECT musician " +
            "FROM MusicianEntity musician " +
            //"WHERE (lower(musician.domain) = lower(:#{#musicianFilter.domain}) OR :#{#musicianFilter.domain}=null) " +
            //"  AND (lower(musician.subdomain) = lower(:#{#musicianFilter.subdomain}) OR :#{#musicianFilter.subdomain}=null) " +
            //"  AND (lower(musician.providerInput) = lower(:#{#musicianFilter.providerInput}) OR :#{#musicianFilter.providerInput}=null) " +
            //"  AND (lower(musician.providerOutput) = lower(:#{#musicianFilter.providerOutput}) OR :#{#musicianFilter.providerOutput}=null) " +
            //"  AND (lower(musician.valueInput) = lower(:#{#musicianFilter.valueInput}) OR :#{#musicianFilter.valueInput}=null) " +
            //"  AND (lower(musician.valueOutput) = lower(:#{#musicianFilter.valueOutput}) OR :#{#musicianFilter.valueOutput}=null) " +
            //"  AND (lower(musician.description) LIKE lower(lower(concat('%',:#{#musicianFilter.description},'%'))) OR :#{#musicianFilter.description}=null) " +
            "ORDER BY musician.id ")
    List<MusicianEntity> findMusicianRegistries(@Param("musicianFilter") FindMusiciansRequest musicianFilter);

}
