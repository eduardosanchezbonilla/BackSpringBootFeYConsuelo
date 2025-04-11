package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventPoiRequest;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.openapi.model.EventPoiRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventPoiRequestDtoToEventPoiRequestConverter {

    public EventPoiRequest convert(final EventPoiRequestDto eventPoiRequestDto) {
        return EventPoiRequest.builder()
                .id(eventPoiRequestDto.getId())
                .center(LatLng.builder()
                        .lat(eventPoiRequestDto.getCenter().getLat())
                        .lng(eventPoiRequestDto.getCenter().getLng())
                        .build()
                )
                .type(eventPoiRequestDto.getType())
                .build();
    }

}
