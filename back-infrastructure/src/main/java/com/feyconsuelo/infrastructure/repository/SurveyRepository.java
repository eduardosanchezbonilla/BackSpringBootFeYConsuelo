package com.feyconsuelo.infrastructure.repository;

import com.feyconsuelo.infrastructure.entities.survey.SurveyEntity;
import com.feyconsuelo.infrastructure.entities.survey.SurveyProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;

public interface SurveyRepository extends JpaRepository<SurveyEntity, Long> {

    @Query("""
             SELECT survey
             FROM SurveyEntity survey
             WHERE survey.deleteDateSurvey Is Null
               And survey.id = :surveyId
            """)
    Optional<SurveyEntity> findActiveBySurveyId(Long surveyId);

    @Query(value = """
             SELECT s.id as id,
                   s.name as name,
                   s.type as type,
                   s.description as description,
                   s.question as question,
                   s.is_public as isPublic,
                   s.is_open as isOpen,
                   s.is_finished as isFinished,
                   s.image_thumbnail as imageThumbnail,
                   s.delete_date as deleteDate,
                   s.creation_date as creationDate,
                   (
                      SELECT json_agg(
                                json_build_object(
                                  'id',so.id,
                                  'name',Trim(so.name),
                                  'description',Trim(so.description),
                                  'youtubeId',Trim(so.youtube_id),
                                  'order',so.option_order
                                )
                              )
                      FROM feyconsuelo.survey_option AS so
                      WHERE so.survey_id = s.id
                         and so.delete_date is null
                     ) AS options,
                     (
                      SELECT count(distinct(username))
                      FROM feyconsuelo.user_point_survey_option AS upso
                      WHERE upso.survey_id = s.id
                         and upso.delete_date is null
                     ) AS numberUsersVote
              FROM feyconsuelo.survey s
              WHERE s.delete_Date Is null
            """,
            nativeQuery = true
    )
    List<SurveyProjection> findAllActives();

    @Query(value = """
             SELECT s.id as id,
                   s.name as name,
                   s.type as type,
                   s.description as description,
                   s.question as question,
                   s.is_public as isPublic,
                   s.is_open as isOpen,
                   s.is_finished as isFinished,
                   s.image_thumbnail as imageThumbnail,
                   s.image as image,
                   s.delete_date as deleteDate,
                   s.creation_date as creationDate,
                   (
                      SELECT json_agg(
                                json_build_object(
                                  'id',so.id,
                                  'name',Trim(so.name),
                                  'description',Trim(so.description),
                                  'youtubeId',Trim(so.youtube_id),
                                  'order',so.option_order
                                )
                              )
                      FROM feyconsuelo.survey_option AS so
                      WHERE so.survey_id = s.id
                         and so.delete_date is null
                     ) AS options,
                     (
                       SELECT json_agg(
                                 json_build_object(
                                   'id',upso.option_id,
                                   'points',upso.points
                                 )
                               )
                       FROM feyconsuelo.user_point_survey_option AS upso
                       WHERE upso.survey_id = s.id
                          and upso.delete_date is null	
                          and upso.username = :username
                     ) AS userOptionsVote,
                     (
                      SELECT count(distinct(username))
                      FROM feyconsuelo.user_point_survey_option AS upso
                      WHERE upso.survey_id = s.id
                         and upso.delete_date is null
                     ) AS numberUsersVote
              FROM feyconsuelo.survey s
              WHERE s.delete_Date Is null
                and s.id = :surveyId
            """,
            nativeQuery = true
    )
    Optional<SurveyProjection> findSurveyActiveById(
            String username,
            Long surveyId
    );

}
