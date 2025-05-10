package com.feyconsuelo.apirest.converter.survey;

import com.feyconsuelo.domain.model.survey.SurveyOption;
import com.feyconsuelo.openapi.model.SurveyOptionDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyOptionToSurveyOptionDtoConverter {

    public SurveyOptionDto convert(final SurveyOption surveyOption) {
        return SurveyOptionDto.builder()
                .id(surveyOption.getId())
                .name(surveyOption.getName())
                .description(surveyOption.getDescription())
                .youtubeId(surveyOption.getYoutubeId())
                .order(surveyOption.getOrder())
                .build();
    }

}
