package com.feyconsuelo.apirest.service.survey.update;

import com.feyconsuelo.apirest.converter.survey.SurveyDtoToSurveyConverter;
import com.feyconsuelo.domain.usecase.survey.UpdateSurvey;
import com.feyconsuelo.openapi.model.SurveyDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateSurveyService {

    private final UpdateSurvey updateSurvey;

    private final SurveyDtoToSurveyConverter surveyDtoToSurveyConverter;

    public ResponseEntity<Void> updateSurvey(final Long surveyId,
                                             final SurveyDto surveyDto) {
        this.updateSurvey.execute(
                surveyId,
                this.surveyDtoToSurveyConverter.convert(surveyDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
