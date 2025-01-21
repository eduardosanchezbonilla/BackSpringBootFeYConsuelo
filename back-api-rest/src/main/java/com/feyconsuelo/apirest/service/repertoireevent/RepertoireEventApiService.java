package com.feyconsuelo.apirest.service.repertoireevent;

import com.feyconsuelo.apirest.service.repertoireevent.delete.DeleteRepertoireEventService;
import com.feyconsuelo.apirest.service.repertoireevent.insert.InsertRepertoireEventService;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.openapi.api.RepertoireEventControllerApiDelegate;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireEventApiService implements RepertoireEventControllerApiDelegate {

    private final InsertRepertoireEventService insertRepertoireEventService;
    private final DeleteRepertoireEventService deleteRepertoireEventService;

    @Override
    public ResponseEntity<Void> deleteRepertoireEvent(final Long marchId,
                                                      final String eventType,
                                                      final Long eventId) {
        return this.deleteRepertoireEventService.deleteRepertoireEvent(
                marchId,
                EventTypeEnum.valueOf(eventType.toUpperCase()),
                eventId
        );
    }

    @Override
    public ResponseEntity<Void> postRepertoireEvent(final Long marchId,
                                                    final String eventType,
                                                    final Long eventId) {
        return this.insertRepertoireEventService.postRepertoireEvent(
                marchId,
                eventType,
                eventId,
                0, // al insertar marcha en repertorio el orden siempre es 0
                1 // al insertar marcha en repertorio el numero siempre es 1
        );
    }

    @Override
    public ResponseEntity<Void> updateRepertoireEvent(final Long marchId,
                                                      final String eventType,
                                                      final Long eventId,
                                                      final Integer order,
                                                      final Integer numbers
    ) {
        return this.insertRepertoireEventService.postRepertoireEvent(
                marchId,
                eventType,
                eventId,
                order,
                numbers
        );
    }

}
