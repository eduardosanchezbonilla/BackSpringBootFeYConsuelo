package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface DeleteEvent {

    void execute(EventTypeEnum eventType, Long eventId);

}
