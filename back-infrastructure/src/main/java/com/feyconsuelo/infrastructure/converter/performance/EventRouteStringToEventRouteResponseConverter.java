package com.feyconsuelo.infrastructure.converter.performance;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.model.event.EventRouteResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class EventRouteStringToEventRouteResponseConverter {

    private final ObjectMapper mapper;

    public EventRouteResponse convert(final String eventRouteString) {
        if (StringUtils.isEmpty(eventRouteString)) {
            return EventRouteResponse.builder().build();
        } else {
            try {
                return this.mapper.readValue(eventRouteString, EventRouteResponse.class);
            } catch (final Exception e) {
                log.error("Error converting route", e);
                return EventRouteResponse.builder().build();
            }
        }
    }
}