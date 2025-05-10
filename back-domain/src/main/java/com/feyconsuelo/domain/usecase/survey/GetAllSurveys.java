package com.feyconsuelo.domain.usecase.survey;

import com.feyconsuelo.domain.model.survey.Survey;

import java.util.List;

public interface GetAllSurveys {

    List<Survey> execute();

}
