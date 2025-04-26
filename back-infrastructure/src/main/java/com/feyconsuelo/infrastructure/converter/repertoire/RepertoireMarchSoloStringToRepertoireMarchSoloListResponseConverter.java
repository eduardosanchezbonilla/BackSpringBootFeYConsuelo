package com.feyconsuelo.infrastructure.converter.repertoire;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSolo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class RepertoireMarchSoloStringToRepertoireMarchSoloListResponseConverter {

    private final ObjectMapper mapper;

    public List<RepertoireMarchSolo> convert(final String repertoireMarchSoloString) {
        if (StringUtils.isEmpty(repertoireMarchSoloString)) {
            return List.of();
        } else {
            try {
                return this.mapper.readValue(repertoireMarchSoloString, new TypeReference<List<RepertoireMarchSolo>>() {
                });
            } catch (final Exception e) {
                log.error("Error converting repertoire march type", e);
                return List.of();
            }
        }
    }
}