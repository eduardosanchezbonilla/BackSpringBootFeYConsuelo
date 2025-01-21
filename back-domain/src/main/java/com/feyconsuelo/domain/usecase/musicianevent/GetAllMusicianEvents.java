package com.feyconsuelo.domain.usecase.musicianevent;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.musicianevent.MusicianEventListResponse;

import java.time.LocalDate;

public interface GetAllMusicianEvents {

    MusicianEventListResponse execute(Long musicianId, LocalDate startDate, LocalDate endDate, EventTypeEnum eventType);

}
