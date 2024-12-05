package com.feyconsuelo.apirest.service.event.delete;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.DeleteEvent;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteEventService {

    private final DeleteEvent deleteEvent;

    public ResponseEntity<Void> deleteEvent(final EventTypeEnum type, final Long eventId) {
        this.deleteEvent.execute(type, eventId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
