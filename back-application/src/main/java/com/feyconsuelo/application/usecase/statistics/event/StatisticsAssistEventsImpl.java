package com.feyconsuelo.application.usecase.statistics.event;

import com.feyconsuelo.application.service.statistiscs.StatisticsAssistEventsService;
import com.feyconsuelo.application.usecase.utils.NumberService;
import com.feyconsuelo.domain.model.statistics.EventAssistStatisticsResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;

@Component
@RequiredArgsConstructor
public class StatisticsAssistEventsImpl {

    private final NumberService numberService;
    private final StatisticsAssistEventsService statisticsAssistEventsService;

    public EventAssistStatisticsResponse getStatisticsAssistEvents(final LocalDate startDate, final LocalDate endDate) {
        return this.statisticsAssistEventsService.getEventAssistStatistics(startDate, endDate);
    }
}
