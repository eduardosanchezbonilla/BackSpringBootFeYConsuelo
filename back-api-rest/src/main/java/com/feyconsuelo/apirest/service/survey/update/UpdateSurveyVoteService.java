package com.feyconsuelo.apirest.service.survey.update;

import com.feyconsuelo.apirest.converter.survey.SurveyVoteDtoToSurveyVoteConverter;
import com.feyconsuelo.domain.usecase.survey.UpdateSurveyVote;
import com.feyconsuelo.openapi.model.SurveyVoteDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateSurveyVoteService {

    private final UpdateSurveyVote updateSurveyVote;

    private final SurveyVoteDtoToSurveyVoteConverter surveyVoteDtoToSurveyVoteConverter;

    public ResponseEntity<Void> voteSurvey(final Long surveyId,
                                           final SurveyVoteDto surveyVoteDto) {
        this.updateSurveyVote.execute(
                surveyId,
                this.surveyVoteDtoToSurveyVoteConverter.convert(surveyVoteDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();

    }
}
