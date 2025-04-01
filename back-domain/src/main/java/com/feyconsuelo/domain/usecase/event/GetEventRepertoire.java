package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.util.Optional;

public interface GetEventRepertoire {

    Optional<EventRepertoireResponse> execute(final EventTypeEnum eventType, final Long eventId, final Boolean returnSolos);

}
