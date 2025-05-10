package com.feyconsuelo.apirest.converter.survey;

import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.openapi.model.SurveyDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyDtoToSurveyConverter {

    private final SurveyOptionDtoToSurveyOptionConverter surveyOptionDtoToSurveyOptionConverter;

    public Survey convert(final SurveyDto surveyDto) {
        return Survey.builder()
                .id(surveyDto.getId())
                .name(surveyDto.getName())
                .type(surveyDto.getType())
                .description(surveyDto.getDescription())
                .question(surveyDto.getQuestion())
                .isPublic(surveyDto.getIsPublic())
                .isOpen(surveyDto.getIsOpen())
                .isFinished(surveyDto.getIsFinished())
                .image(surveyDto.getImage())
                .createdTime(surveyDto.getCreatedTime())
                .options(
                        CollectionUtils.isEmpty(surveyDto.getOptions()) ?
                                List.of() :
                                surveyDto.getOptions().stream()
                                        .map(this.surveyOptionDtoToSurveyOptionConverter::convert)
                                        .toList()
                )
                .build();
    }

}
