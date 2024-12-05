package com.feyconsuelo.apirest.service.event.insert;

import com.feyconsuelo.apirest.converter.event.EventRequestDtoToEventRequestConverter;
import com.feyconsuelo.domain.usecase.event.InsertEvent;
import com.feyconsuelo.openapi.model.EventRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertEventService {

    private final InsertEvent insertEvent;

    private final EventRequestDtoToEventRequestConverter eventRequestDtoToEventRequestConverter;

    public ResponseEntity<Void> postEvent(final EventRequestDto eventRequestDto) {
        this.insertEvent.execute(
                this.eventRequestDtoToEventRequestConverter.convert(eventRequestDto)
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }
}
