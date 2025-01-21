package com.feyconsuelo.application.service.statistiscs;

import com.feyconsuelo.domain.model.statistics.EventAssistStatisticsResponse;

import java.time.LocalDate;

public interface StatisticsAssistEventsService {

    EventAssistStatisticsResponse getEventAssistStatistics(
            LocalDate startDate,
            LocalDate endDate
    );

}
