package com.feyconsuelo.domain.usecase.survey;

import com.feyconsuelo.domain.model.survey.Survey;

public interface UpdateSurvey {

    void execute(Long surveyId, Survey survey);

}
