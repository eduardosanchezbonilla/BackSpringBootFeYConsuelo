package com.feyconsuelo.infrastructure.converter.repertoirerehearsal;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventRepertoireStringToEventRepertoireResponseConverter {

    private final ObjectMapper mapper;

    public EventRepertoireResponse convert(final String repertoire) {
        if (StringUtils.isEmpty(repertoire)) {
            return null;
        } else {
            try {
                return this.mapper.readValue(repertoire, EventRepertoireResponse.class);
            } catch (final Exception e) {
                log.error("Error converting repertoire march type", e);
                return null;
            }
        }
    }
}
