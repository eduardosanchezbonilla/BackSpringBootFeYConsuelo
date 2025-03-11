package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.openapi.model.EventCrossheadDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventCrossheadDtoToEventCrossheadConverter {

    private final EventCrossheadStreetDtoToEventCrossheadStreetConverter eventCrossheadStreetDtoToEventCrossheadStreetConverter;

    public EventCrosshead convert(final EventCrossheadDto eventCrossheadDto) {
        return EventCrosshead.builder()
                .streets(
                        CollectionUtils.isEmpty(eventCrossheadDto.getStreets()) ?
                                null :
                                eventCrossheadDto.getStreets().stream()
                                        .map(this.eventCrossheadStreetDtoToEventCrossheadStreetConverter::convert)
                                        .toList()
                )
                .build();
    }

}
