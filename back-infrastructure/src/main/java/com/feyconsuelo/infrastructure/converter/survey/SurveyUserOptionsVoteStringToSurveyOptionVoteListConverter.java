package com.feyconsuelo.infrastructure.converter.survey;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.survey.SurveyOptionVote;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class SurveyUserOptionsVoteStringToSurveyOptionVoteListConverter {

    private final ObjectMapper mapper;

    public List<SurveyOptionVote> convert(final String surveyUserOptionsVoteString) {
        if (StringUtils.isEmpty(surveyUserOptionsVoteString)) {
            return List.of();
        } else {
            try {
                return this.mapper.readValue(surveyUserOptionsVoteString, new TypeReference<List<SurveyOptionVote>>() {
                });
            } catch (final Exception e) {
                log.error("Error converting survey options vote", e);
                return List.of();
            }
        }
    }
}