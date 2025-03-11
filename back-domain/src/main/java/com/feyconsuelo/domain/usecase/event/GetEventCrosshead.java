package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.util.Optional;

public interface GetEventCrosshead {

    Optional<EventCrosshead> execute(EventTypeEnum eventType, Long eventId);

}
