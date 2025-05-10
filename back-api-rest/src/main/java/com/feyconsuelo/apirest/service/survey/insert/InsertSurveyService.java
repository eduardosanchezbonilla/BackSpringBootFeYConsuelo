package com.feyconsuelo.apirest.service.survey.insert;

import com.feyconsuelo.apirest.converter.survey.SurveyDtoToSurveyConverter;
import com.feyconsuelo.domain.usecase.survey.InsertSurvey;
import com.feyconsuelo.openapi.model.SurveyDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertSurveyService {

    private final InsertSurvey insertSurvey;

    private final SurveyDtoToSurveyConverter surveyDtoToSurveyConverter;

    public ResponseEntity<Void> insertSurvey(final SurveyDto surveyDto) {
        this.insertSurvey.execute(
                this.surveyDtoToSurveyConverter.convert(surveyDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
