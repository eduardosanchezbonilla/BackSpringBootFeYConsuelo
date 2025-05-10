package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.survey.UserPointSurveyOptionEntity;
import com.feyconsuelo.infrastructure.entities.survey.UserPointSurveyOptionPK;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface UserPointSurveyOptionRepository extends JpaRepository<UserPointSurveyOptionEntity, UserPointSurveyOptionPK> {

    @Modifying
    @Query("""
             DELETE
             FROM UserPointSurveyOptionEntity userPointSurveyOption
             WHERE userPointSurveyOption.id.username = :username
               AND userPointSurveyOption.id.surveyId = :surveyId
            """)
    void deleteAllOptionsSurveyByUsernameAndSurveyId(String username, Long surveyId);

    @Modifying
    @Query("""
             DELETE
             FROM UserPointSurveyOptionEntity userPointSurveyOption
             WHERE userPointSurveyOption.id.surveyId = :surveyId
            """)
    void deleteAllOptionsSurveyBySurveyId(Long surveyId);
}
