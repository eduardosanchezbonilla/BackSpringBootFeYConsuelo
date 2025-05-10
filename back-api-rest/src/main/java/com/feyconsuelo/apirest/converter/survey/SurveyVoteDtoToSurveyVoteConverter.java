package com.feyconsuelo.apirest.converter.survey;

import com.feyconsuelo.domain.model.survey.SurveyVote;
import com.feyconsuelo.openapi.model.SurveyVoteDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyVoteDtoToSurveyVoteConverter {

    private final SurveyOptionVoteDtoToSurveyOptionVoteConverter surveyOptionVoteDtoToSurveyOptionVoteConverter;

    public SurveyVote convert(final SurveyVoteDto surveyVoteDto) {
        return SurveyVote.builder()
                .id(surveyVoteDto.getId())
                .options(
                        CollectionUtils.isEmpty(surveyVoteDto.getOptions()) ?
                                List.of() :
                                surveyVoteDto.getOptions().stream()
                                        .map(this.surveyOptionVoteDtoToSurveyOptionVoteConverter::convert)
                                        .toList()
                )
                .build();
    }

}
