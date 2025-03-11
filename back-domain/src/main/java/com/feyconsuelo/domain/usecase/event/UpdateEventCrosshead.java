package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface UpdateEventCrosshead {

    void execute(EventTypeEnum eventType, Long eventId, EventCrosshead eventCrosshead);

}
