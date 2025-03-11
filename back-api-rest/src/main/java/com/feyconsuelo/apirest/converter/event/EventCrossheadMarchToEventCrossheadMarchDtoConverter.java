package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventCrossheadMarch;
import com.feyconsuelo.openapi.model.EventCrossheadMarchDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventCrossheadMarchToEventCrossheadMarchDtoConverter {

    public EventCrossheadMarchDto convert(final EventCrossheadMarch eventCrossheadMarch) {
        return EventCrossheadMarchDto.builder()
                .id(eventCrossheadMarch.getId())
                .marchId(eventCrossheadMarch.getMarchId())
                .marchName(eventCrossheadMarch.getMarchName())
                .marchOrder(eventCrossheadMarch.getMarchOrder())
                .annotations(eventCrossheadMarch.getAnnotations())
                .build();
    }

}
