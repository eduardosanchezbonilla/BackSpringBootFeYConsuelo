package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;

import java.time.LocalDate;

public interface GetAllEvents {

    MusicianEventListResponse execute(LocalDate startDate, LocalDate endDate, EventTypeEnum eventType);

}
