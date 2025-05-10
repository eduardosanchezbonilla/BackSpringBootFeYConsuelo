package com.feyconsuelo.apirest.service.survey.query;

import com.feyconsuelo.apirest.converter.survey.SurveyToSurveyDtoConverter;
import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.domain.usecase.survey.GetAllSurveys;
import com.feyconsuelo.domain.usecase.survey.GetSurvey;
import com.feyconsuelo.openapi.model.SurveyDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetSurveyService {

    private final GetAllSurveys getAllSurveys;

    private final GetSurvey getSurvey;

    private final SurveyToSurveyDtoConverter surveyToSurveyDtoConverter;


    public ResponseEntity<List<SurveyDto>> getAllSurveys() {
        final List<Survey> surveyList = this.getAllSurveys.execute();
        if (CollectionUtils.isEmpty(surveyList)) {
            return ResponseEntity.noContent().build();
        } else {
            return ResponseEntity.ok(
                    surveyList.stream()
                            .map(this.surveyToSurveyDtoConverter::convert)
                            .sorted(Comparator.comparing(SurveyDto::getCreatedTime).reversed())
                            .toList()
            );
        }
    }

    public ResponseEntity<SurveyDto> getSurvey(final Long surveyId) {
        final Optional<SurveyDto> surveyDto = this.getSurvey.execute(surveyId).map(this.surveyToSurveyDtoConverter::convert);
        return surveyDto.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

}
