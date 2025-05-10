package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.survey.SurveyOptionEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface SurveyOptionRepository extends JpaRepository<SurveyOptionEntity, Long> {

    @Query("""
             SELECT surveyOption
             FROM SurveyOptionEntity surveyOption
             WHERE surveyOption.deleteDateSurveyOption Is Null
               And surveyOption.surveyId = :surveyId
            """)
    List<SurveyOptionEntity> findOptionActiveBySurveyId(Long surveyId);

    @Modifying
    @Query("""
             DELETE
             FROM SurveyOptionEntity surveyOption
             WHERE surveyOption.surveyId = :surveyId
                AND surveyOption.id NOT IN :surveyOptionIds
            """)
    void deleteAllOptionsSurvey(
            Long surveyId,
            List<Long> surveyOptionIds
    );
}
