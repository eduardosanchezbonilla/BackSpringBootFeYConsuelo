package com.feyconsuelo.apirest.service.event.query;

import com.feyconsuelo.apirest.converter.event.EventResponseListToEventResponseDtoListConverter;
import com.feyconsuelo.apirest.converter.event.EventResponseToEventResponseDtoConverter;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.GetAllEvents;
import com.feyconsuelo.domain.usecase.event.GetEvent;
import com.feyconsuelo.openapi.model.EventResponseDto;
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

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;

    private final EventResponseListToEventResponseDtoListConverter eventResponseListToEventResponseDtoListConverter;

    public ResponseEntity<List<EventResponseDto>> getAllEvents(final EventTypeEnum eventType, final LocalDate startDate, final LocalDate endDate) {
        final List<EventResponse> eventResponseList = this.getAllEvents.execute(startDate, endDate, eventType);
        if (CollectionUtils.isEmpty(eventResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.eventResponseListToEventResponseDtoListConverter.convert(eventResponseList));
    }

    public ResponseEntity<EventResponseDto> getEvent(final EventTypeEnum eventType, final Long eventId) {
        final Optional<EventResponseDto> event = this.getEvent.execute(eventType, eventId).map(this.eventResponseToEventResponseDtoConverter::convert);
        return event.map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.noContent().build());
    }


}
