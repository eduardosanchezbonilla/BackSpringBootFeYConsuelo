package com.feyconsuelo.apirest.service.repertoireevent.insert;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.domain.usecase.repertoireevent.InsertRepertoireEvent;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class InsertRepertoireEventService {

    private final InsertRepertoireEvent insertRepertoireEvent;

    public ResponseEntity<Void> postRepertoireEvent(final Long marchId,
                                                    final String eventType,
                                                    final Long eventId,
                                                    final Integer order,
                                                    final Integer numbers

    ) {
        this.insertRepertoireEvent.execute(
                RepertoireEventRequest.builder()
                        .marchId(marchId)
                        .eventId(eventId)
                        .eventType(EventTypeEnum.valueOf(eventType.toUpperCase()))
                        .order(order)
                        .numbers(numbers)
                        .build()
        );
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

}
