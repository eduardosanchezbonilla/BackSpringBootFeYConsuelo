package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface SuggestionBoxRepository extends JpaRepository<SuggestionBoxEntity, Long> {

    @Query("""
             SELECT suggestionBoxEntity
             FROM SuggestionBoxEntity suggestionBoxEntity
             WHERE suggestionBoxEntity.deleteDateSuggestionBox Is Null
             ORDER BY suggestionBoxEntity.user.username
            """)
    List<SuggestionBoxEntity> findAllActives();

    @Query("""
             SELECT suggestionBoxEntity
             FROM SuggestionBoxEntity suggestionBoxEntity
             WHERE suggestionBoxEntity.deleteDateSuggestionBox Is Null
               And suggestionBoxEntity.id = :id
            """)
    Optional<SuggestionBoxEntity> findActiveById(Long id);

    @Query("""
             SELECT suggestionBoxEntity
             FROM SuggestionBoxEntity suggestionBoxEntity
             WHERE suggestionBoxEntity.deleteDateSuggestionBox Is Null
               And suggestionBoxEntity.user.username = :username
             ORDER BY suggestionBoxEntity.id
            """)
    List<SuggestionBoxEntity> findAllActivesByUser(String username);

}
