package com.feyconsuelo.apirest.service.event;

import com.feyconsuelo.apirest.service.event.delete.DeleteEventService;
import com.feyconsuelo.apirest.service.event.insert.InsertEventService;
import com.feyconsuelo.apirest.service.event.query.GetEventService;
import com.feyconsuelo.apirest.service.event.report.GetEventReportAssistanceService;
import com.feyconsuelo.apirest.service.event.update.UpdateEventService;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.openapi.api.EventControllerApiDelegate;
import com.feyconsuelo.openapi.model.EventFormationRequestDto;
import com.feyconsuelo.openapi.model.EventGroupByAnyoResponseDto;
import com.feyconsuelo.openapi.model.EventMusicianAssistanceResponseDto;
import com.feyconsuelo.openapi.model.EventRepertoireResponseDto;
import com.feyconsuelo.openapi.model.EventReportAssistanceResponseDto;
import com.feyconsuelo.openapi.model.EventRequestDto;
import com.feyconsuelo.openapi.model.EventResponseDto;
import com.feyconsuelo.openapi.model.MusicianEventListResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class EventApiService implements EventControllerApiDelegate {

    private final InsertEventService insertEventService;
    private final DeleteEventService deleteEventService;
    private final UpdateEventService updateEventService;
    private final GetEventService getEventService;
    private final GetEventReportAssistanceService getEventReportAssistanceService;


    @Override
    public ResponseEntity<Void> deleteEvent(final String eventType, final Long eventId) {
        return this.deleteEventService.deleteEvent(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<Void> postEvent(final EventRequestDto eventRequestDto) {
        return this.insertEventService.postEvent(eventRequestDto);
    }

    @Override
    public ResponseEntity<Void> updateEvent(final String eventType,
                                            final Long eventId,
                                            final EventRequestDto eventRequestDto
    ) {
        return this.updateEventService.updateEvent(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId,
                eventRequestDto
        );
    }

    @Override
    public ResponseEntity<MusicianEventListResponseDto> getAllEvents(final String eventType,
                                                                     final LocalDate startDate,
                                                                     final LocalDate endDate) {
        return this.getEventService.getAllEvents(
                StringUtils.isEmpty(eventType) ? null : EventTypeEnum.valueOf(eventType.toUpperCase()),
                startDate,
                endDate
        );
    }

    @Override
    public ResponseEntity<List<EventGroupByAnyoResponseDto>> getEventsGroupByAnyo(final String eventType,
                                                                                  final LocalDate startDate,
                                                                                  final LocalDate endDate,
                                                                                  final String name
    ) {
        return this.getEventService.getEventGroupByAnyo(
                StringUtils.isEmpty(eventType) ? null : EventTypeEnum.valueOf(eventType.toUpperCase()),
                startDate,
                endDate,
                name
        );
    }

    @Override
    public ResponseEntity<EventResponseDto> getEvent(final String eventType,
                                                     final Long eventId) {
        return this.getEventService.getEvent(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<EventMusicianAssistanceResponseDto> getEventMusicianAssistance(final String eventType,
                                                                                         final Long eventId
    ) {
        return this.getEventService.getEventMusicianAssistance(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<EventRepertoireResponseDto> getEventRepertoire(final String eventType,
                                                                         final Long eventId
    ) {
        return this.getEventService.getEventRepertoire(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<EventReportAssistanceResponseDto> eventReportAssistance(final String eventType,
                                                                                  final Long eventId) {
        return this.getEventReportAssistanceService.getEventReportAssistance(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<Void> updateEventFormation(final String eventType,
                                                     final Long eventId,
                                                     final EventFormationRequestDto eventFormationRequestDto
    ) {
        return this.updateEventService.updateEventFormation(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId,
                eventFormationRequestDto
        );
    }

}
