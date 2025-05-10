package com.feyconsuelo.apirest.converter.survey;

import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.openapi.model.SurveyDto;
import com.feyconsuelo.openapi.model.SurveyOptionDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Comparator;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyToSurveyDtoConverter {

    private final SurveyOptionToSurveyOptionDtoConverter surveyOptionToSurveyOptionDtoConverter;
    private final SurveyOptionVoteToSurveyOptionVoteDtoConverter surveyOptionVoteToSurveyOptionVoteDtoConverter;

    public SurveyDto convert(final Survey survey) {
        return SurveyDto.builder()
                .id(survey.getId())
                .name(survey.getName())
                .type(survey.getType())
                .description(survey.getDescription())
                .question(survey.getQuestion())
                .isPublic(survey.getIsPublic())
                .isOpen(survey.getIsOpen())
                .isFinished(survey.getIsFinished())
                .image(survey.getImage())
                .createdTime(survey.getCreatedTime())
                .options(
                        CollectionUtils.isEmpty(survey.getOptions()) ?
                                List.of() :
                                survey.getOptions().stream()
                                        .map(this.surveyOptionToSurveyOptionDtoConverter::convert)
                                        .sorted(Comparator.comparing(SurveyOptionDto::getOrder))
                                        .toList()
                )
                .userOptionsVote(
                        CollectionUtils.isEmpty(survey.getOptions()) ?
                                List.of() :
                                survey.getUserOptionsVote().stream()
                                        .map(this.surveyOptionVoteToSurveyOptionVoteDtoConverter::convert)
                                        .toList()
                )
                .numberUsersVote(survey.getNumberUsersVote())
                .build();
    }

}
