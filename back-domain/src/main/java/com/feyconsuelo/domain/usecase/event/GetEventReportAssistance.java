package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventReportAssistanceResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.util.Optional;

public interface GetEventReportAssistance {

    Optional<EventReportAssistanceResponse> execute(final EventTypeEnum eventType, final Long eventId);

}
