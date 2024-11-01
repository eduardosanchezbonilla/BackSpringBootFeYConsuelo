package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.userpartiturerequest.UserPartitureRequestEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface UserPartitureRequestRepository extends JpaRepository<UserPartitureRequestEntity, Long> {

    @Query("""
             SELECT userPartitureRequestEntity
             FROM UserPartitureRequestEntity userPartitureRequestEntity
             WHERE userPartitureRequestEntity.deleteDatePartitureRequest Is Null
             ORDER BY userPartitureRequestEntity.user.username
            """)
    List<UserPartitureRequestEntity> findAllActives();

    @Query("""
             SELECT userPartitureRequestEntity
              FROM UserPartitureRequestEntity userPartitureRequestEntity
             WHERE userPartitureRequestEntity.deleteDatePartitureRequest Is Null
               And userPartitureRequestEntity.id = :id
            """)
    Optional<UserPartitureRequestEntity> findActiveById(Long id);

    @Query("""
             SELECT userPartitureRequestEntity
              FROM UserPartitureRequestEntity userPartitureRequestEntity
             WHERE userPartitureRequestEntity.deleteDatePartitureRequest Is Null
               And userPartitureRequestEntity.user.username = :username
             ORDER BY userPartitureRequestEntity.id
            """)
    List<UserPartitureRequestEntity> findAllActivesByUser(String username);

}
