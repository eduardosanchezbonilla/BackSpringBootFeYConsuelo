package com.feyconsuelo.apirest.converter.survey;

import com.feyconsuelo.domain.model.survey.SurveyOption;
import com.feyconsuelo.openapi.model.SurveyOptionDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyOptionDtoToSurveyOptionConverter {

    public SurveyOption convert(final SurveyOptionDto surveyOptionDto) {
        return SurveyOption.builder()
                .id(surveyOptionDto.getId())
                .name(surveyOptionDto.getName())
                .description(surveyOptionDto.getDescription())
                .youtubeId(surveyOptionDto.getYoutubeId())
                .order(surveyOptionDto.getOrder())
                .build();
    }

}
