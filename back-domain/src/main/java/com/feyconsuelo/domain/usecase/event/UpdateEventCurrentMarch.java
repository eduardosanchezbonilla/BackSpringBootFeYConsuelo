package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface UpdateEventCurrentMarch {

    void execute(EventTypeEnum eventType, Long eventId, String march);

}
