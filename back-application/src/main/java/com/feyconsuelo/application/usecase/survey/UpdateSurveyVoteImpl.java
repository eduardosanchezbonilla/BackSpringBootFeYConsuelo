package com.feyconsuelo.application.usecase.survey;

import com.feyconsuelo.application.service.survey.SurveyService;
import com.feyconsuelo.application.service.user.TokenInfoExtractorService;
import com.feyconsuelo.domain.model.survey.SurveyVote;
import com.feyconsuelo.domain.usecase.survey.UpdateSurveyVote;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateSurveyVoteImpl implements UpdateSurveyVote {

    private final SurveyService surveyService;
    private final TokenInfoExtractorService tokenInfoExtractorService;

    @Override
    public void execute(final Long surveyId, final SurveyVote surveyVote) {
        surveyVote.setUsername(this.tokenInfoExtractorService.getUsername());
        this.surveyService.vote(surveyId, surveyVote);
    }

}
