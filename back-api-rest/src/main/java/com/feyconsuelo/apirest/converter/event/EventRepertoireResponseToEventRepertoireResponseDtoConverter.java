package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.apirest.converter.repertoire.RepertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter;
import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.openapi.model.EventRepertoireResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventRepertoireResponseToEventRepertoireResponseDtoConverter {

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;
    private final RepertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter repertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter;

    public EventRepertoireResponseDto convert(final EventRepertoireResponse eventRepertoireResponse) {
        return EventRepertoireResponseDto.builder()
                .event(this.eventResponseToEventResponseDtoConverter.convert(eventRepertoireResponse.getEvent()))
                .repertoireMarchGroupByType(this.repertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter.convert(eventRepertoireResponse.getMarchs()))
                .build();
    }

}
