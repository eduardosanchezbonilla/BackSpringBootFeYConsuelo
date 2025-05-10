package com.feyconsuelo.application.service.survey;

import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.model.survey.SurveyVote;

import java.util.List;
import java.util.Optional;

public interface SurveyService {

    void delete(Long surveyId);

    void logicalDelete(Long surveyId);

    List<Survey> getAll();

    Optional<Survey> get(Long surveyId, Boolean isThumbnail);

    void insert(Survey survey);

    void update(Long surveyId, Survey survey);

    void vote(Long surveyId, SurveyVote surveyVote);

}
