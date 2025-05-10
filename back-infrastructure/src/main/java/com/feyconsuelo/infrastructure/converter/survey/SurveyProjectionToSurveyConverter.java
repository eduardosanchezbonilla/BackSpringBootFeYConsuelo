package com.feyconsuelo.infrastructure.converter.survey;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.survey.Survey;
import com.feyconsuelo.infrastructure.entities.survey.SurveyProjection;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyProjectionToSurveyConverter {

    private static final String TIME_FORMAT = "dd/MM/yyyy HH:mm";
    private final DateService dateService;
    private final SurveyOptionsStringToSurveyOptionListConverter surveyOptionsStringToSurveyOptionListConverter;
    private final SurveyUserOptionsVoteStringToSurveyOptionVoteListConverter surveyUserOptionsVoteStringToSurveyOptionVoteListConverter;

    public Survey convert(final SurveyProjection surveyProjection,
                          final Boolean isThumbnail) {
        return Survey.builder()
                .id(surveyProjection.getId())
                .name(surveyProjection.getName())
                .type(surveyProjection.getType())
                .description(surveyProjection.getDescription())
                .question(surveyProjection.getQuestion())
                .isPublic(surveyProjection.getIsPublic())
                .isOpen(surveyProjection.getIsOpen())
                .isFinished(surveyProjection.getIsFinished())
                .image(Boolean.TRUE.equals(isThumbnail) ? surveyProjection.getImageThumbnail() : surveyProjection.getImage())
                .createdTime(this.dateService.dateToString(surveyProjection.getCreationDate(), DateTimeFormatter.ofPattern(TIME_FORMAT)))
                .options(this.surveyOptionsStringToSurveyOptionListConverter.convert(surveyProjection.getOptions()))
                .userOptionsVote(this.surveyUserOptionsVoteStringToSurveyOptionVoteListConverter.convert(surveyProjection.getUserOptionsVote()))
                .numberUsersVote(surveyProjection.getNumberUsersVote())
                .build();
    }

}
