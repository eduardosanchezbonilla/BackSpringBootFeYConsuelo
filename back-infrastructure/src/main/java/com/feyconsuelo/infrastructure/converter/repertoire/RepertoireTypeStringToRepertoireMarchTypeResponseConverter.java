package com.feyconsuelo.infrastructure.converter.repertoire;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class RepertoireTypeStringToRepertoireMarchTypeResponseConverter {

    private final ObjectMapper mapper;

    public RepertoireMarchTypeResponse convert(final String repertoireMarchTypeString) {
        if (StringUtils.isEmpty(repertoireMarchTypeString)) {
            return RepertoireMarchTypeResponse.builder().build();
        } else {
            try {
                return this.mapper.readValue(repertoireMarchTypeString, RepertoireMarchTypeResponse.class);
            } catch (final Exception e) {
                log.error("Error converting repertoire march type", e);
                return RepertoireMarchTypeResponse.builder().build();
            }
        }
    }
}