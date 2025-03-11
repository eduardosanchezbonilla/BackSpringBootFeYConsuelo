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
public class EventCrossheadStreetDtoToEventCrossheadStreetConverter {

    private final EventCrossheadMarchDtoToEventCrossheadMarchConverter eventCrossheadMarchDtoToEventCrossheadMarchConverter;

    public EventCrossheadStreet convert(final EventCrossheadStreetDto eventCrossheadStreetDto) {
        return EventCrossheadStreet.builder()
                .id(eventCrossheadStreetDto.getId())
                .street(eventCrossheadStreetDto.getStreet())
                .streetOrder(eventCrossheadStreetDto.getStreetOrder())
                .annotations(eventCrossheadStreetDto.getAnnotations())
                .marchs(
                        CollectionUtils.isEmpty(eventCrossheadStreetDto.getMarchs()) ?
                                null :
                                eventCrossheadStreetDto.getMarchs().stream()
                                        .map(this.eventCrossheadMarchDtoToEventCrossheadMarchConverter::convert)
                                        .toList()
                )
                .build();
    }

}
