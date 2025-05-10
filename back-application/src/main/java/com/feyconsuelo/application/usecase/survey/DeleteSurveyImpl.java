package com.feyconsuelo.application.usecase.survey;

import com.feyconsuelo.application.service.survey.SurveyService;
import com.feyconsuelo.domain.usecase.survey.DeleteSurvey;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteSurveyImpl implements DeleteSurvey {

    private final SurveyService surveyService;

    @Override
    public void execute(final Long surveyId) {
        this.surveyService.logicalDelete(surveyId);
    }

}
