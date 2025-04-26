package com.feyconsuelo.infrastructure.converter.repertoire;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class RepertoireMarchGroupByTypeStringToRepertoireMarchGroupByTypeResponseConverter {

    private final ObjectMapper mapper;

    public List<RepertoireMarchGroupByTypeResponse> convert(final String value) {
        if (StringUtils.isEmpty(value)) {
            return List.of();
        } else {
            try {
                return this.mapper.readValue(value, new TypeReference<List<RepertoireMarchGroupByTypeResponse>>() {
                });
            } catch (final Exception e) {
                log.error("Error converting marchs", e);
                return List.of();
            }
        }
    }
}