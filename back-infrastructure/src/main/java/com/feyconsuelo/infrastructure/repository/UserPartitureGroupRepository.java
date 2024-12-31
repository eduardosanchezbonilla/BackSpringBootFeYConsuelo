package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupEntity;
import com.feyconsuelo.infrastructure.entities.userpartituregroup.UserPartitureGroupPK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserPartitureGroupRepository extends JpaRepository<UserPartitureGroupEntity, UserPartitureGroupPK> {

    @Query("""
             SELECT userPartitureGroupEntity
             FROM UserPartitureGroupEntity userPartitureGroupEntity
             WHERE userPartitureGroupEntity.deleteDate Is Null
             ORDER BY userPartitureGroupEntity.id.username
            """)
    List<UserPartitureGroupEntity> findAllActives();

    @Query("""
             SELECT userPartitureGroupEntity
             FROM UserPartitureGroupEntity userPartitureGroupEntity
             WHERE userPartitureGroupEntity.deleteDate Is Null
               And userPartitureGroupEntity.id.username = :username
             ORDER BY userPartitureGroupEntity.id.username
            """)
    List<UserPartitureGroupEntity> findAllActivesByUser(String username);

    @Query("""
             SELECT userPartitureGroupEntity
             FROM UserPartitureGroupEntity userPartitureGroupEntity
             WHERE userPartitureGroupEntity.id.username = :username
               And userPartitureGroupEntity.id.partitureGroupId = :partitureGroupId
            """)
    Optional<UserPartitureGroupEntity> findUserPartitureGroupActiveById(
            String username,
            Long partitureGroupId
    );

    @Query("""
             SELECT userPartitureGroupEntity
             FROM UserPartitureGroupEntity userPartitureGroupEntity
             WHERE userPartitureGroupEntity.deleteDate Is Null
               And userPartitureGroupEntity.id.partitureGroupId = :partitureGroupId
            """)
    List<UserPartitureGroupEntity> findUserWithPartitureGroup(Long partitureGroupId);

}
