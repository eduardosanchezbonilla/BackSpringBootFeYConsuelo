package com.feyconsuelo.apirest.service.repertoireevent.delete;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.repertoireevent.DeleteRepertoireEvent;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteRepertoireEventService {

    private final DeleteRepertoireEvent deleteRepertoireEvent;

    public ResponseEntity<Void> deleteRepertoireEvent(final Long marchId, final EventTypeEnum type, final Long eventId) {
        this.deleteRepertoireEvent.execute(marchId, type, eventId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

}
