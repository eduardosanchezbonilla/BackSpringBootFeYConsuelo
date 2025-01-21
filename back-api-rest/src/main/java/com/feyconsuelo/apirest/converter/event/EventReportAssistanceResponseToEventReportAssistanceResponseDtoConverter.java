package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.apirest.converter.repertoire.RepertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter;
import com.feyconsuelo.domain.model.event.EventReportAssistanceResponse;
import com.feyconsuelo.openapi.model.EventReportAssistanceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventReportAssistanceResponseToEventReportAssistanceResponseDtoConverter {

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;
    private final RepertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter repertoireMarchGroupByTypeResponseListToRepertoireMarchGroupByTypeResponseDtoListConverter;

    public EventReportAssistanceResponseDto convert(final EventReportAssistanceResponse eventReportAssistanceResponse) {
        return EventReportAssistanceResponseDto.builder()
                .event(this.eventResponseToEventResponseDtoConverter.convert(eventReportAssistanceResponse.getEvent()))
                .report(eventReportAssistanceResponse.getReport())
                .build();
    }

}
