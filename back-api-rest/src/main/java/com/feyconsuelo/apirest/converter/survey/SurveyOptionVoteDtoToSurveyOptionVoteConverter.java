package com.feyconsuelo.apirest.converter.survey;

import com.feyconsuelo.domain.model.survey.SurveyOptionVote;
import com.feyconsuelo.openapi.model.SurveyOptionVoteDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class SurveyOptionVoteDtoToSurveyOptionVoteConverter {

    public SurveyOptionVote convert(final SurveyOptionVoteDto surveyOptionVoteDto) {
        return SurveyOptionVote.builder()
                .id(surveyOptionVoteDto.getId())
                .points(surveyOptionVoteDto.getScore())
                .build();
    }

}
