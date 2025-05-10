package com.feyconsuelo.apirest.service.survey.delete;

import com.feyconsuelo.domain.usecase.survey.DeleteSurvey;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteSurveyService {

    private final DeleteSurvey deleteSurvey;

    public ResponseEntity<Void> deleteSurvey(final Long surveyId) {
        this.deleteSurvey.execute(surveyId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
