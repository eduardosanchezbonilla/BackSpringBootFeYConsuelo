package com.feyconsuelo.application.usecase.survey;

import com.feyconsuelo.application.service.survey.SurveyService;
import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.usecase.survey.GetAllSurveys;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllSurveysImpl implements GetAllSurveys {

    private final SurveyService surveyService;

    @Override
    public List<Survey> execute() {
        return this.surveyService.getAll();
    }
}
