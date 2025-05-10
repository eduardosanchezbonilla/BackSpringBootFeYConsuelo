package com.feyconsuelo.infrastructure.converter.survey;

import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.infrastructure.entities.survey.SurveyProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyProjectionListToSurveyListConverter {

    private final SurveyProjectionToSurveyConverter surveyProjectionToSurveyConverter;

    public List<Survey> convert(final List<SurveyProjection> surveyProjectionList,
                                final Boolean isThumbnail) {
        if (CollectionUtils.isEmpty(surveyProjectionList)) {
            return List.of();
        } else {
            return surveyProjectionList.stream()
                    .map(surveyProjection -> this.surveyProjectionToSurveyConverter.convert(surveyProjection, isThumbnail))
                    .toList();
        }
    }

}
