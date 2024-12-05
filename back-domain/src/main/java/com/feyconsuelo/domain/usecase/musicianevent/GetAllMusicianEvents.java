package com.feyconsuelo.domain.usecase.musicianevent;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;

import java.time.LocalDate;
import java.util.List;

public interface GetAllMusicianEvents {

    List<EventResponse> execute(Long musicianId, LocalDate startDate, LocalDate endDate, EventTypeEnum eventType);

}
