package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventRouteResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.util.Optional;

public interface GetEventRoute {

    Optional<EventRouteResponse> execute(EventTypeEnum eventType, Long eventId);

}
