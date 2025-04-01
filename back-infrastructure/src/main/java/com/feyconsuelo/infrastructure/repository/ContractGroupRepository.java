package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.contractgroup.ContractGroupEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface ContractGroupRepository extends JpaRepository<ContractGroupEntity, Long> {

    @Query("""
             SELECT contractGroupEntity
             FROM ContractGroupEntity contractGroupEntity
             WHERE contractGroupEntity.deleteDate Is Null
                AND (contractGroupEntity.id In :contractGroupIdList
                      OR
                      :allContractGroups = True
                    )
            """)
    List<ContractGroupEntity> findAllActives(List<Long> contractGroupIdList,
                                             Boolean allContractGroups
    );

    @Query("""
             SELECT contractGroupEntity
             FROM ContractGroupEntity contractGroupEntity
             WHERE contractGroupEntity.deleteDate Is Null
               And contractGroupEntity.id = :contractGroupId
            """)
    Optional<ContractGroupEntity> findContractGroupActiveById(Long contractGroupId);

}
