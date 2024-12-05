package com.feyconsuelo.apirest.service.event.update;

import com.feyconsuelo.apirest.converter.event.EventRequestDtoToEventRequestConverter;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.UpdateEvent;
import com.feyconsuelo.openapi.model.EventRequestDto;
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

    private final EventRequestDtoToEventRequestConverter eventRequestDtoToEventRequestConverter;


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
}
