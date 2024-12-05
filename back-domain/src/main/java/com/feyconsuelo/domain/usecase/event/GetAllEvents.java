package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.time.LocalDate;
import java.util.List;

public interface GetAllEvents {

    List<EventResponse> execute(LocalDate startDate, LocalDate endDate, EventTypeEnum eventType);

}
