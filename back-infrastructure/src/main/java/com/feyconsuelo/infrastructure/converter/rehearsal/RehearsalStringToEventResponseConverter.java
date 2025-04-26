package com.feyconsuelo.infrastructure.converter.rehearsal;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class RehearsalStringToEventResponseConverter {

    private final ObjectMapper mapper;

    public EventResponse convert(final String rehearsalString) {
        if (StringUtils.isEmpty(rehearsalString)) {
            return null;
        } else {
            try {
                return this.mapper.readValue(rehearsalString, new TypeReference<EventResponse>() {
                });
            } catch (final Exception e) {
                log.error("Error converting rehearsal", e);
                return null;
            }
        }
    }
}