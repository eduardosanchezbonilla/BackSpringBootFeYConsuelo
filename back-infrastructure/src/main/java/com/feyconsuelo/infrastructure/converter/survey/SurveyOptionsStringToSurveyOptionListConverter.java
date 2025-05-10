package com.feyconsuelo.infrastructure.converter.survey;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.survey.SurveyOption;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class SurveyOptionsStringToSurveyOptionListConverter {

    private final ObjectMapper mapper;

    public List<SurveyOption> convert(final String surveyOptionsString) {
        if (StringUtils.isEmpty(surveyOptionsString)) {
            return List.of();
        } else {
            try {
                return this.mapper.readValue(surveyOptionsString, new TypeReference<List<SurveyOption>>() {
                });
            } catch (final Exception e) {
                log.error("Error converting survey options", e);
                return List.of();
            }
        }
    }
}