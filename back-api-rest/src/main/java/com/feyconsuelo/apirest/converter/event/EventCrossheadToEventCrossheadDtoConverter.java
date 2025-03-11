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
public class EventCrossheadToEventCrossheadDtoConverter {

    private final EventCrossheadStreetToEventCrossheadStreetDtoConverter eventCrossheadStreetToEventCrossheadStreetDtoConverter;

    public EventCrossheadDto convert(final EventCrosshead eventCrosshead) {
        return EventCrossheadDto.builder()
                .streets(
                        CollectionUtils.isEmpty(eventCrosshead.getStreets()) ?
                                null :
                                eventCrosshead.getStreets().stream()
                                        .map(this.eventCrossheadStreetToEventCrossheadStreetDtoConverter::convert)
                                        .toList()
                )
                .build();
    }

}
