package com.feyconsuelo.domain.usecase.survey;

import com.feyconsuelo.domain.model.survey.Survey;

import java.util.Optional;

public interface GetSurvey {

    Optional<Survey> execute(Long surveyId);

}
