package com.feyconsuelo.domain.usecase.repertoireevent;

import com.feyconsuelo.domain.model.event.EventTypeEnum;

public interface DeleteRepertoireEvent {

    void execute(Long marchId, EventTypeEnum eventType, Long eventId);

}
