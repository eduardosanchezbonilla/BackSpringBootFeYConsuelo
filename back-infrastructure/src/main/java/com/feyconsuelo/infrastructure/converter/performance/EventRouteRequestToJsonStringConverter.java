package com.feyconsuelo.infrastructure.converter.performance;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventRouteRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class EventRouteRequestToJsonStringConverter {

    private final ObjectMapper mapper;

    public String convert(final EventRouteRequest eventRouteRequest) {
        try {
            return this.mapper.writeValueAsString(eventRouteRequest);
        } catch (final Exception e) {
            throw new FeYConsueloException("Error converting model", e);
        }
    }
}