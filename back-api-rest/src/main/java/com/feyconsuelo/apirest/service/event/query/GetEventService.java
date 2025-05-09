package com.feyconsuelo.apirest.service.event.query;

import com.feyconsuelo.apirest.converter.event.EventCrossheadToEventCrossheadDtoConverter;
import com.feyconsuelo.apirest.converter.event.EventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter;
import com.feyconsuelo.apirest.converter.event.EventRepertoireResponseToEventRepertoireResponseDtoConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseListToEventGroupByAnyoResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseListToEventResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseToEventResponseDtoConverter;
import com.feyconsuelo.apirest.converter.event.EventRouteResponseToEventRouteResponseDtoConverter;
import com.feyconsuelo.apirest.converter.event.GlobalEventStatsResponseToGlobalEventStatsResponseDtoConverter;
import com.feyconsuelo.apirest.converter.musicianevent.MusicianEventListResponseToMusicianEventListResponseDtoConverter;
import com.feyconsuelo.domain.model.event.EventCurrentDataResponse;
import com.feyconsuelo.domain.model.event.EventMusicianAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
import com.feyconsuelo.domain.usecase.event.GetAllEvents;
import com.feyconsuelo.domain.usecase.event.GetEvent;
import com.feyconsuelo.domain.usecase.event.GetEventCrosshead;
import com.feyconsuelo.domain.usecase.event.GetEventCurrentData;
import com.feyconsuelo.domain.usecase.event.GetEventCurrentPosition;
import com.feyconsuelo.domain.usecase.event.GetEventMusicianAssistance;
import com.feyconsuelo.domain.usecase.event.GetEventRepertoire;
import com.feyconsuelo.domain.usecase.event.GetEventRoute;
import com.feyconsuelo.domain.usecase.event.GetEventStats;
import com.feyconsuelo.openapi.model.EventCrossheadDto;
import com.feyconsuelo.openapi.model.EventCurrentDataResponseDto;
import com.feyconsuelo.openapi.model.EventGroupByAnyoResponseDto;
import com.feyconsuelo.openapi.model.EventMusicianAssistanceResponseDto;
import com.feyconsuelo.openapi.model.EventRepertoireResponseDto;
import com.feyconsuelo.openapi.model.EventResponseDto;
import com.feyconsuelo.openapi.model.EventRouteResponseDto;
import com.feyconsuelo.openapi.model.GlobalEventStatsResponseDto;
import com.feyconsuelo.openapi.model.LatLngResponseDto;
import com.feyconsuelo.openapi.model.MusicianEventListResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetEventService {

    private final GetAllEvents getAllEvents;

    private final GetEvent getEvent;

    private final GetEventStats getEventStats;

    private final GetEventCurrentPosition getEventCurrentPosition;

    private final GetEventCurrentData getEventCurrentData;

    private final GetEventRoute getEventRoute;

    private final GetEventCrosshead getEventCrosshead;

    private final GetEventMusicianAssistance getEventMusicianAssistance;

    private final GetEventRepertoire getEventRepertoire;

    private final EventCrossheadToEventCrossheadDtoConverter eventCrossheadToEventCrossheadDtoConverter;

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;

    private final EventResponseListToEventResponseDtoListConverter eventResponseListToEventResponseDtoListConverter;

    private final EventResponseListToEventGroupByAnyoResponseDtoListConverter eventResponseListToEventGroupByAnyoResponseDtoListConverter;

    private final EventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter eventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter;

    private final EventRepertoireResponseToEventRepertoireResponseDtoConverter eventRepertoireResponseToEventRepertoireResponseDtoConverter;

    private final MusicianEventListResponseToMusicianEventListResponseDtoConverter musicianEventListResponseToMusicianEventListResponseDtoConverter;

    private final EventRouteResponseToEventRouteResponseDtoConverter eventRouteResponseToEventRouteResponseDtoConverter;

    private final GlobalEventStatsResponseToGlobalEventStatsResponseDtoConverter globalEventStatsResponseToGlobalEventStatsResponseDtoConverter;

    public ResponseEntity<MusicianEventListResponseDto> getAllEvents(final EventTypeEnum eventType, final LocalDate startDate, final LocalDate endDate, final Boolean allEvents) {
        final MusicianEventListResponse musicianEventListResponse = this.getAllEvents.execute(startDate, endDate, eventType, Boolean.FALSE);
        return ResponseEntity.ok(this.musicianEventListResponseToMusicianEventListResponseDtoConverter.convert(musicianEventListResponse, allEvents));
    }

    public ResponseEntity<EventResponseDto> getEvent(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventResponseDto> event = this.getEvent.execute(eventType, eventId).map(this.eventResponseToEventResponseDtoConverter::convert);
        return event.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<List<EventGroupByAnyoResponseDto>> getEventGroupByAnyo(final EventTypeEnum eventType,
                                                                                 final LocalDate startDate,
                                                                                 final LocalDate endDate,
                                                                                 final String name,
                                                                                 final Boolean isTodayPerformance) {
        final List<EventResponse> eventResponseList = this.getAllEvents.execute(startDate, endDate, eventType, isTodayPerformance).getEvents();
        if (CollectionUtils.isEmpty(eventResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.eventResponseListToEventGroupByAnyoResponseDtoListConverter.convert(eventResponseList, name));
    }

    public ResponseEntity<EventMusicianAssistanceResponseDto> getEventMusicianAssistance(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventMusicianAssistanceResponse> eventMusicianAssistanceResponse = this.getEventMusicianAssistance.execute(eventType, eventId, Boolean.FALSE);
        return eventMusicianAssistanceResponse.map(musicianAssistanceResponse -> ResponseEntity.ok(this.eventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter.convert(musicianAssistanceResponse))).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<EventMusicianAssistanceResponseDto> getEventMusicianFormation(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventMusicianAssistanceResponse> eventMusicianAssistanceResponse = this.getEventMusicianAssistance.execute(eventType, eventId, Boolean.TRUE);
        return eventMusicianAssistanceResponse.map(musicianAssistanceResponse -> ResponseEntity.ok(this.eventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter.convert(musicianAssistanceResponse))).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<EventRepertoireResponseDto> getEventRepertoire(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventRepertoireResponse> eventRepertoireResponse = this.getEventRepertoire.execute(eventType, eventId);
        return eventRepertoireResponse.map(event -> ResponseEntity.ok(this.eventRepertoireResponseToEventRepertoireResponseDtoConverter.convert(event))).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<LatLngResponseDto> getEventCurrentPosition(final EventTypeEnum eventType, final Long eventId) {
        final Optional<LatLng> latLng = this.getEventCurrentPosition.execute(eventType, eventId);

        return latLng.map(lng -> ResponseEntity.ok(LatLngResponseDto.builder()
                .lat(lng.getLat())
                .lng(lng.getLng())
                .build()
        )).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<EventRouteResponseDto> getEventRoute(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventRouteResponseDto> event = this.getEventRoute.execute(eventType, eventId).map(this.eventRouteResponseToEventRouteResponseDtoConverter::convert);
        return event.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<EventCurrentDataResponseDto> getEventCurrentData(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventCurrentDataResponse> currentDataResponse = this.getEventCurrentData.execute(eventType, eventId);

        return currentDataResponse.map(data -> ResponseEntity.ok(EventCurrentDataResponseDto.builder()
                .lat(data.getLat())
                .lng(data.getLng())
                .march(data.getMarch())
                .build()
        )).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<EventCrossheadDto> getEventCrosshead(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventCrossheadDto> event = this.getEventCrosshead.execute(eventType, eventId).map(this.eventCrossheadToEventCrossheadDtoConverter::convert);
        return event.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<GlobalEventStatsResponseDto> getEventStats(final Boolean excludeSpecialTypes, final EventTypeEnum eventType, final LocalDate startDate, final LocalDate endDate) {
        final Optional<GlobalEventStatsResponseDto> stats = this.getEventStats.execute(excludeSpecialTypes, eventType, startDate, endDate).map(this.globalEventStatsResponseToGlobalEventStatsResponseDtoConverter::convert);
        return stats.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }
}
