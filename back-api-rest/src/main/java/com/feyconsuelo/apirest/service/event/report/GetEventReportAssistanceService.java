package com.feyconsuelo.apirest.service.event.report;

import com.feyconsuelo.apirest.converter.event.EventReportAssistanceResponseToEventReportAssistanceResponseDtoConverter;
import com.feyconsuelo.domain.model.event.EventReportAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.GetEventReportAssistance;
import com.feyconsuelo.openapi.model.EventReportAssistanceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetEventReportAssistanceService {

    private final EventReportAssistanceResponseToEventReportAssistanceResponseDtoConverter eventRepertoireResponseToEventRepertoireResponseDtoConverter;
    private final GetEventReportAssistance getEventReportAssistance;

    public ResponseEntity<EventReportAssistanceResponseDto> getEventReportAssistance(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventReportAssistanceResponse> eventReportAssistanceResponse = this.getEventReportAssistance.execute(eventType, eventId);
        return eventReportAssistanceResponse.map(event -> ResponseEntity.ok(this.eventRepertoireResponseToEventRepertoireResponseDtoConverter.convert(event))).orElseGet(() -> ResponseEntity.noContent().build());
    }


}
