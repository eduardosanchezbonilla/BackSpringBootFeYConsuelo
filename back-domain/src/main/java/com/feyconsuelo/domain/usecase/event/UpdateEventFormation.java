package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface UpdateEventFormation {

    void execute(EventTypeEnum eventType, Long eventId, EventFormationRequest eventFormationRequest);

}
