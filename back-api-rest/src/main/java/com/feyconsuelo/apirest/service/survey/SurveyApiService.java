package com.feyconsuelo.apirest.service.survey;

import com.feyconsuelo.apirest.service.survey.delete.DeleteSurveyService;
import com.feyconsuelo.apirest.service.survey.insert.InsertSurveyService;
import com.feyconsuelo.apirest.service.survey.query.GetSurveyService;
import com.feyconsuelo.apirest.service.survey.update.UpdateSurveyService;
import com.feyconsuelo.apirest.service.survey.update.UpdateSurveyVoteService;
import com.feyconsuelo.openapi.api.SurveyControllerApiDelegate;
import com.feyconsuelo.openapi.model.SurveyDto;
import com.feyconsuelo.openapi.model.SurveyVoteDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class SurveyApiService implements SurveyControllerApiDelegate {

    private final DeleteSurveyService deleteSurveyService;
    private final InsertSurveyService insertSurveyService;
    private final UpdateSurveyService updateSurveyService;
    private final GetSurveyService getSurveyService;
    private final UpdateSurveyVoteService updateSurveyVoteService;

    @Override
    public ResponseEntity<Void> deleteSurvey(final Long surveyId) {
        return this.deleteSurveyService.deleteSurvey(surveyId);
    }

    @Override
    public ResponseEntity<Void> insertSurvey(final SurveyDto surveyDto) {
        return this.insertSurveyService.insertSurvey(surveyDto);
    }

    @Override
    public ResponseEntity<Void> updateSurvey(final Long surveyId,
                                             final SurveyDto surveyDto) {
        return this.updateSurveyService.updateSurvey(surveyId, surveyDto);
    }


    @Override
    public ResponseEntity<List<SurveyDto>> getAllSurveys() {
        return this.getSurveyService.getAllSurveys();
    }

    @Override
    public ResponseEntity<SurveyDto> getSurvey(final Long surveyId) {
        return this.getSurveyService.getSurvey(surveyId);
    }

    @Override
    public ResponseEntity<Void> voteSurvey(final Long surveyId,
                                           final SurveyVoteDto surveyVoteDto) {
        return this.updateSurveyVoteService.voteSurvey(surveyId, surveyVoteDto);
    }

}
