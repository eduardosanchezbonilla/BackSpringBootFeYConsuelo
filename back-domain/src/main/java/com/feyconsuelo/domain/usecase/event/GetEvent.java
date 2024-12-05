package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.util.Optional;

public interface GetEvent {

    Optional<EventResponse> execute(EventTypeEnum eventType, Long eventId);

}
