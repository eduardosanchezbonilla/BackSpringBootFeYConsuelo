package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.partituregroup.PartitureGroupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface PartitureGroupRepository extends JpaRepository<PartitureGroupEntity, Long> {

    @Query("""
             SELECT partitureGroupRequest
             FROM PartitureGroupEntity partitureGroupRequest
             WHERE partitureGroupRequest.deleteDate Is Null
                AND (partitureGroupRequest.id In :partitureGroupIdList
                      OR 
                      :allPartitureGroups = True
                    )
            """)
    List<PartitureGroupEntity> findAllActives(List<Long> partitureGroupIdList,
                                              Boolean allPartitureGroups
    );

    @Query("""
             SELECT partitureGroupRequest
             FROM PartitureGroupEntity partitureGroupRequest
             WHERE partitureGroupRequest.deleteDate Is Null
               And partitureGroupRequest.id = :partitureGroupId
            """)
    Optional<PartitureGroupEntity> findPartitureGroupActiveById(Long partitureGroupId);

}
