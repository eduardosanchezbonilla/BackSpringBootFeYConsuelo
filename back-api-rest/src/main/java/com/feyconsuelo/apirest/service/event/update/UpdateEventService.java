package com.feyconsuelo.apirest.service.event.update;

import com.feyconsuelo.apirest.converter.event.EventCrossheadDtoToEventCrossheadConverter;
import com.feyconsuelo.apirest.converter.event.EventFormationRequestDtoToEventFormationRequestConverter;
import com.feyconsuelo.apirest.converter.event.EventRequestDtoToEventRequestConverter;
import com.feyconsuelo.apirest.converter.event.EventRouteRequestDtoToEventRouteRequestConverter;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.usecase.event.UpdateEvent;
import com.feyconsuelo.domain.usecase.event.UpdateEventCrosshead;
import com.feyconsuelo.domain.usecase.event.UpdateEventCurrentMarch;
import com.feyconsuelo.domain.usecase.event.UpdateEventCurrentPosition;
import com.feyconsuelo.domain.usecase.event.UpdateEventFormation;
import com.feyconsuelo.domain.usecase.event.UpdateEventRoute;
import com.feyconsuelo.openapi.model.CurrentMarchRequestDto;
import com.feyconsuelo.openapi.model.EventCrossheadDto;
import com.feyconsuelo.openapi.model.EventFormationRequestDto;
import com.feyconsuelo.openapi.model.EventRequestDto;
import com.feyconsuelo.openapi.model.EventRouteRequestDto;
import com.feyconsuelo.openapi.model.LatLngRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateEventService {

    private final UpdateEvent updateEvent;

    private final UpdateEventFormation updateEventFormation;

    private final UpdateEventRoute updateEventRoute;

    private final UpdateEventCurrentPosition updateEventCurrentPosition;

    private final UpdateEventCurrentMarch updateEventCurrentMarch;

    private final UpdateEventCrosshead updateEventCrosshead;

    private final EventCrossheadDtoToEventCrossheadConverter eventCrossheadDtoToEventCrossheadConverter;

    private final EventRequestDtoToEventRequestConverter eventRequestDtoToEventRequestConverter;

    private final EventRouteRequestDtoToEventRouteRequestConverter eventRouteRequestDtoToEventRouteRequestConverter;

    private final EventFormationRequestDtoToEventFormationRequestConverter eventFormationRequestDtoToEventFormationRequestConverter;


    public ResponseEntity<Void> updateEvent(final EventTypeEnum type,
                                            final Long eventId,
                                            final EventRequestDto eventRequestDto) {
        this.updateEvent.execute(
                type,
                eventId,
                this.eventRequestDtoToEventRequestConverter.convert(eventRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    public ResponseEntity<Void> updateEventFormation(final EventTypeEnum type,
                                                     final Long eventId,
                                                     final EventFormationRequestDto eventFormationRequestDto) {
        this.updateEventFormation.execute(
                type,
                eventId,
                this.eventFormationRequestDtoToEventFormationRequestConverter.convert(eventFormationRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    public ResponseEntity<Void> updateEventRoute(final EventTypeEnum type,
                                                 final Long eventId,
                                                 final EventRouteRequestDto eventRouteRequestDto) {
        this.updateEventRoute.execute(
                type,
                eventId,
                this.eventRouteRequestDtoToEventRouteRequestConverter.convert(eventRouteRequestDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    public ResponseEntity<Void> updateEventCurrentPosition(final EventTypeEnum type,
                                                           final Long eventId,
                                                           final LatLngRequestDto latLngRequestDto) {
        this.updateEventCurrentPosition.execute(
                type,
                eventId,
                LatLng.builder()
                        .lat(latLngRequestDto.getLat())
                        .lng(latLngRequestDto.getLng())
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    public ResponseEntity<Void> updateEventCurrentMarch(final EventTypeEnum type,
                                                        final Long eventId,
                                                        final CurrentMarchRequestDto currentMarchRequestDto) {
        this.updateEventCurrentMarch.execute(
                type,
                eventId,
                currentMarchRequestDto.getMarch()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    public ResponseEntity<Void> updateEventCrosshead(final EventTypeEnum type,
                                                     final Long eventId,
                                                     final EventCrossheadDto eventCrossheadDto) {
        this.updateEventCrosshead.execute(
                type,
                eventId,
                this.eventCrossheadDtoToEventCrossheadConverter.convert(eventCrossheadDto)
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }


}
