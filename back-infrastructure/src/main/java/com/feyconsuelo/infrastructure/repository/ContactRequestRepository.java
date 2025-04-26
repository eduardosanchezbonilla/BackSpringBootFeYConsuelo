package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.contactrequest.ContactRequestEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface ContactRequestRepository extends JpaRepository<ContactRequestEntity, Long> {

    @Query("""
             SELECT contactRequestEntity
             FROM ContactRequestEntity contactRequestEntity
             WHERE contactRequestEntity.deleteDateContactRequest Is Null
             ORDER BY contactRequestEntity.createdDateContactRequest DESC 
            """)
    List<ContactRequestEntity> findAllActives();

    @Query("""
             SELECT contactRequestEntity
             FROM ContactRequestEntity contactRequestEntity
             WHERE contactRequestEntity.deleteDateContactRequest Is Null
               And contactRequestEntity.id = :id
            """)
    Optional<ContactRequestEntity> findActiveById(Long id);

}
