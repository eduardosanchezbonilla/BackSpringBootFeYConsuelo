package com.feyconsuelo.apirest.service.event.query;

import com.feyconsuelo.apirest.converter.event.EventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter;
import com.feyconsuelo.apirest.converter.event.EventRepertoireResponseToEventRepertoireResponseDtoConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseListToEventGroupByAnyoResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseListToEventResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseToEventResponseDtoConverter;
import com.feyconsuelo.apirest.converter.musicianevent.MusicianEventListResponseToMusicianEventListResponseDtoConverter;
import com.feyconsuelo.domain.model.event.EventMusicianAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;
import com.feyconsuelo.domain.usecase.event.GetAllEvents;
import com.feyconsuelo.domain.usecase.event.GetEvent;
import com.feyconsuelo.domain.usecase.event.GetEventMusicianAssistance;
import com.feyconsuelo.domain.usecase.event.GetEventRepertoire;
import com.feyconsuelo.openapi.model.EventGroupByAnyoResponseDto;
import com.feyconsuelo.openapi.model.EventMusicianAssistanceResponseDto;
import com.feyconsuelo.openapi.model.EventRepertoireResponseDto;
import com.feyconsuelo.openapi.model.EventResponseDto;
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

    private final GetEventMusicianAssistance getEventMusicianAssistance;

    private final GetEventRepertoire getEventRepertoire;

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;

    private final EventResponseListToEventResponseDtoListConverter eventResponseListToEventResponseDtoListConverter;

    private final EventResponseListToEventGroupByAnyoResponseDtoListConverter eventResponseListToEventGroupByAnyoResponseDtoListConverter;

    private final EventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter eventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter;

    private final EventRepertoireResponseToEventRepertoireResponseDtoConverter eventRepertoireResponseToEventRepertoireResponseDtoConverter;

    private final MusicianEventListResponseToMusicianEventListResponseDtoConverter musicianEventListResponseToMusicianEventListResponseDtoConverter;

    public ResponseEntity<MusicianEventListResponseDto> getAllEvents(final EventTypeEnum eventType, final LocalDate startDate, final LocalDate endDate) {
        final MusicianEventListResponse musicianEventListResponse = this.getAllEvents.execute(startDate, endDate, eventType);
        return ResponseEntity.ok(this.musicianEventListResponseToMusicianEventListResponseDtoConverter.convert(musicianEventListResponse));
    }

    public ResponseEntity<EventResponseDto> getEvent(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventResponseDto> event = this.getEvent.execute(eventType, eventId).map(this.eventResponseToEventResponseDtoConverter::convert);
        return event.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<List<EventGroupByAnyoResponseDto>> getEventGroupByAnyo(final EventTypeEnum eventType, final LocalDate startDate, final LocalDate endDate, final String name) {
        final List<EventResponse> eventResponseList = this.getAllEvents.execute(startDate, endDate, eventType).getEvents();
        if (CollectionUtils.isEmpty(eventResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.eventResponseListToEventGroupByAnyoResponseDtoListConverter.convert(eventResponseList, name));
    }

    public ResponseEntity<EventMusicianAssistanceResponseDto> getEventMusicianAssistance(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventMusicianAssistanceResponse> eventMusicianAssistanceResponse = this.getEventMusicianAssistance.execute(eventType, eventId);
        return eventMusicianAssistanceResponse.map(musicianAssistanceResponse -> ResponseEntity.ok(this.eventMusicianAssistanceResponseToEventMusicianAssistanceResponseDtoConverter.convert(musicianAssistanceResponse))).orElseGet(() -> ResponseEntity.noContent().build());
    }

    public ResponseEntity<EventRepertoireResponseDto> getEventRepertoire(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventRepertoireResponse> eventRepertoireResponse = this.getEventRepertoire.execute(eventType, eventId);
        return eventRepertoireResponse.map(event -> ResponseEntity.ok(this.eventRepertoireResponseToEventRepertoireResponseDtoConverter.convert(event))).orElseGet(() -> ResponseEntity.noContent().build());
    }


}
