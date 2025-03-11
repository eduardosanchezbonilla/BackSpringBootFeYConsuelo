package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventCrossheadStreet;
import com.feyconsuelo.openapi.model.EventCrossheadStreetDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventCrossheadStreetToEventCrossheadStreetDtoConverter {

    private final EventCrossheadMarchToEventCrossheadMarchDtoConverter eventCrossheadMarchToEventCrossheadMarchDtoConverter;

    public EventCrossheadStreetDto convert(final EventCrossheadStreet eventCrossheadStreet) {
        return EventCrossheadStreetDto.builder()
                .id(eventCrossheadStreet.getId())
                .street(eventCrossheadStreet.getStreet())
                .streetOrder(eventCrossheadStreet.getStreetOrder())
                .annotations(eventCrossheadStreet.getAnnotations())
                .marchs(
                        CollectionUtils.isEmpty(eventCrossheadStreet.getMarchs()) ?
                                null :
                                eventCrossheadStreet.getMarchs().stream()
                                        .map(this.eventCrossheadMarchToEventCrossheadMarchDtoConverter::convert)
                                        .toList()
                )
                .build();
    }

}
