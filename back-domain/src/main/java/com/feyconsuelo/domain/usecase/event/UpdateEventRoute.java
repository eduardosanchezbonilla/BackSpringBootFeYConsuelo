package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventRouteRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface UpdateEventRoute {

    void execute(EventTypeEnum eventType, Long eventId, EventRouteRequest eventRouteRequest);

}
