package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventPoiResponse;
import com.feyconsuelo.openapi.model.EventPoiResponseDto;
import com.feyconsuelo.openapi.model.LatLngRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventPoiResponseDtoToEventPoiResponseConverter {

    public EventPoiResponseDto convert(final EventPoiResponse eventPoiResponse) {
        return EventPoiResponseDto.builder()
                .id(eventPoiResponse.getId())
                .center(LatLngRequestDto.builder()
                        .lat(eventPoiResponse.getCenter().getLat())
                        .lng(eventPoiResponse.getCenter().getLng())
                        .build()
                )
                .type(eventPoiResponse.getType())
                .build();
    }

}
