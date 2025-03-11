package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventCrossheadMarch;
import com.feyconsuelo.openapi.model.EventCrossheadMarchDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventCrossheadMarchDtoToEventCrossheadMarchConverter {

    public EventCrossheadMarch convert(final EventCrossheadMarchDto eventCrossheadMarchDto) {
        return EventCrossheadMarch.builder()
                .id(eventCrossheadMarchDto.getId())
                .marchId(eventCrossheadMarchDto.getMarchId())
                .marchName(eventCrossheadMarchDto.getMarchName())
                .marchOrder(eventCrossheadMarchDto.getMarchOrder())
                .annotations(eventCrossheadMarchDto.getAnnotations())
                .build();
    }

}
