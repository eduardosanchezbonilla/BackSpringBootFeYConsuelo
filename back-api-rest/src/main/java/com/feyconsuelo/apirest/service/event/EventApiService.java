package com.feyconsuelo.apirest.service.event;

import com.feyconsuelo.apirest.service.event.delete.DeleteEventService;
import com.feyconsuelo.apirest.service.event.insert.InsertEventService;
import com.feyconsuelo.apirest.service.event.query.GetEventService;
import com.feyconsuelo.apirest.service.event.report.GetEventReportAssistanceService;
import com.feyconsuelo.apirest.service.event.update.UpdateEventService;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.openapi.api.EventControllerApiDelegate;
import com.feyconsuelo.openapi.model.CurrentMarchRequestDto;
import com.feyconsuelo.openapi.model.EventCrossheadDto;
import com.feyconsuelo.openapi.model.EventCurrentDataResponseDto;
import com.feyconsuelo.openapi.model.EventFormationRequestDto;
import com.feyconsuelo.openapi.model.EventGroupByAnyoResponseDto;
import com.feyconsuelo.openapi.model.EventMusicianAssistanceResponseDto;
import com.feyconsuelo.openapi.model.EventRepertoireResponseDto;
import com.feyconsuelo.openapi.model.EventReportAssistanceResponseDto;
import com.feyconsuelo.openapi.model.EventRequestDto;
import com.feyconsuelo.openapi.model.EventResponseDto;
import com.feyconsuelo.openapi.model.EventRouteRequestDto;
import com.feyconsuelo.openapi.model.EventRouteResponseDto;
import com.feyconsuelo.openapi.model.GlobalEventStatsResponseDto;
import com.feyconsuelo.openapi.model.LatLngRequestDto;
import com.feyconsuelo.openapi.model.LatLngResponseDto;
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
    public ResponseEntity<MusicianEventListResponseDto> getAllEvents(final Boolean allEvents,
                                                                     final String eventType,
                                                                     final LocalDate startDate,
                                                                     final LocalDate endDate) {
        return this.getEventService.getAllEvents(
                StringUtils.isEmpty(eventType) ? null : EventTypeEnum.valueOf(eventType.toUpperCase()),
                startDate,
                endDate,
                allEvents
        );
    }

    @Override
    public ResponseEntity<List<EventGroupByAnyoResponseDto>> getEventsGroupByAnyo(final String eventType,
                                                                                  final LocalDate startDate,
                                                                                  final LocalDate endDate,
                                                                                  final String name,
                                                                                  final Boolean isTodayPerformance
    ) {
        return this.getEventService.getEventGroupByAnyo(
                StringUtils.isEmpty(eventType) ? null : EventTypeEnum.valueOf(eventType.toUpperCase()),
                startDate,
                endDate,
                name,
                isTodayPerformance == null ? Boolean.FALSE : isTodayPerformance
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
    public ResponseEntity<EventMusicianAssistanceResponseDto> getEventMusicianFormation(final String eventType,
                                                                                        final Long eventId
    ) {
        return this.getEventService.getEventMusicianFormation(
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

    @Override
    public ResponseEntity<Void> updateEventRoute(final String eventType,
                                                 final Long eventId,
                                                 final EventRouteRequestDto eventRouteRequestDto
    ) {
        return this.updateEventService.updateEventRoute(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId,
                eventRouteRequestDto
        );
    }

    @Override
    public ResponseEntity<Void> updateEventCurrentPosition(final String eventType,
                                                           final Long eventId,
                                                           final LatLngRequestDto latLngRequestDto
    ) {
        return this.updateEventService.updateEventCurrentPosition(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId,
                latLngRequestDto
        );
    }

    @Override
    public ResponseEntity<LatLngResponseDto> getEventCurrentPosition(final String eventType,
                                                                     final Long eventId) {
        return this.getEventService.getEventCurrentPosition(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<EventRouteResponseDto> getEventRoute(final String eventType,
                                                               final Long eventId) {
        return this.getEventService.getEventRoute(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<Void> updateEventCurrentMarch(final String eventType,
                                                        final Long eventId,
                                                        final CurrentMarchRequestDto currentMarchRequestDto
    ) {
        return this.updateEventService.updateEventCurrentMarch(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId,
                currentMarchRequestDto
        );
    }

    @Override
    public ResponseEntity<EventCurrentDataResponseDto> getEventCurrentData(final String eventType,
                                                                           final Long eventId) {
        return this.getEventService.getEventCurrentData(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<EventCrossheadDto> getEventCrooshead(final String eventType,
                                                               final Long eventId) {
        return this.getEventService.getEventCrosshead(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<Void> updateEventCrosshead(final String eventType,
                                                     final Long eventId,
                                                     final EventCrossheadDto eventCrossheadDto
    ) {
        return this.updateEventService.updateEventCrosshead(
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId,
                eventCrossheadDto
        );
    }

    @Override
    public ResponseEntity<GlobalEventStatsResponseDto> getEventStats(final Boolean excludeSpecialTypes,
                                                                     final String eventType,
                                                                     final LocalDate startDate,
                                                                     final LocalDate endDate) {
        return this.getEventService.getEventStats(
                excludeSpecialTypes,
                StringUtils.isEmpty(eventType) ? null : EventTypeEnum.valueOf(eventType.toUpperCase()),
                startDate,
                endDate
        );
    }
}
