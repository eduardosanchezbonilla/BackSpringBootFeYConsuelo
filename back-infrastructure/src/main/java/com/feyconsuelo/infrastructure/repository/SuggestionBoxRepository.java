package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxEntity;
import com.feyconsuelo.infrastructure.entities.suggestionbox.SuggestionBoxProjection;
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

    @Query(value = """
             select sb.id as id,
                   sb.suggestion as suggestion,
                   sb.readed as readed,
                   sb.delete_date as delete_date,
                   u.username as username,
                   u.dni as dni,
                   u.name as name,
                   u.surname as surname,
                   u.direction as direction,
                   u.municipality as municipality,
                   u.province as province,
                   u.email as email,
                   u.description as description,
                   u.image_thumbnail as image,
                   u.phonenumber as phoneNumber,
                   sb.creation_date as creationDate
               from feyconsuelo.suggestion_box sb,
                 feyconsuelo.user u
               where sb.username = u.username
                 and sb.delete_date is null
               order by sb.username
            """,
            nativeQuery = true)
    List<SuggestionBoxProjection> findAllActivesProjection();

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
