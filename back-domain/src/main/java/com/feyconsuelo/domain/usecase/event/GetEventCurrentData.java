package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventCurrentDataResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.util.Optional;

public interface GetEventCurrentData {

    Optional<EventCurrentDataResponse> execute(EventTypeEnum eventType, Long eventId);

}
