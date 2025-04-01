package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.GlobalEventStatsResponse;

import java.time.LocalDate;
import java.util.Optional;

public interface GetEventStats {

    Optional<GlobalEventStatsResponse> execute(Boolean excludeSpecialTypes, EventTypeEnum eventType, LocalDate startDate, LocalDate endDate);

}
