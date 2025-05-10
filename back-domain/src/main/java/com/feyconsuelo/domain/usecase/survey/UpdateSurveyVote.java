package com.feyconsuelo.domain.usecase.survey;

import com.feyconsuelo.domain.model.survey.SurveyVote;

public interface UpdateSurveyVote {

    void execute(Long surveyId, SurveyVote survey);

}
