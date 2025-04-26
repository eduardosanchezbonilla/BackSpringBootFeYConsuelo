package com.feyconsuelo.infrastructure.converter.repertoire;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class RepertoireCategoryStringToRepertoireCategoryResponseConverter {

    private final ObjectMapper mapper;

    public RepertoireCategoryResponse convert(final String repertoireCategoryString) {
        if (StringUtils.isEmpty(repertoireCategoryString)) {
            return RepertoireCategoryResponse.builder().build();
        } else {
            try {
                return this.mapper.readValue(repertoireCategoryString, RepertoireCategoryResponse.class);
            } catch (final Exception e) {
                log.error("Error converting repertoire category", e);
                return RepertoireCategoryResponse.builder().build();
            }
        }
    }
}