package com.feyconsuelo.infrastructure.converter.survey;

import com.feyconsuelo.domain.model.survey.SurveyVote;
import com.feyconsuelo.infrastructure.entities.survey.UserPointSurveyOptionEntity;
import com.feyconsuelo.infrastructure.entities.survey.UserPointSurveyOptionPK;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyVoteToUserPointSurveyVoteEntityListConverter {

    public List<UserPointSurveyOptionEntity> convert(final SurveyVote surveyVote) {
        return surveyVote.getOptions().stream()
                .map(option -> UserPointSurveyOptionEntity.builder()
                        .id(
                                UserPointSurveyOptionPK.builder()
                                        .username(surveyVote.getUsername())
                                        .surveyId(surveyVote.getId())
                                        .optionId(option.getId())
                                        .build()
                        )
                        .points(option.getPoints())
                        .modifiedUserSurveyVoteOption(surveyVote.getUsername())
                        .build()
                )
                .toList();
    }

}
