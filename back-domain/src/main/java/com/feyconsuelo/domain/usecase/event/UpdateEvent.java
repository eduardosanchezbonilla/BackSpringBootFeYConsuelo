package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface UpdateEvent {

    void execute(EventTypeEnum eventType, Long eventId, EventRequest eventRequest);

}
