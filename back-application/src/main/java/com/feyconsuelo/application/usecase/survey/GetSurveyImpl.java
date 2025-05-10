package com.feyconsuelo.application.usecase.survey;

import com.feyconsuelo.application.service.survey.SurveyService;
import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.usecase.survey.GetSurvey;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetSurveyImpl implements GetSurvey {

    private final SurveyService surveyService;

    @Override
    public Optional<Survey> execute(final Long surveyId) {
        return this.surveyService.get(surveyId, Boolean.FALSE);
    }
}
