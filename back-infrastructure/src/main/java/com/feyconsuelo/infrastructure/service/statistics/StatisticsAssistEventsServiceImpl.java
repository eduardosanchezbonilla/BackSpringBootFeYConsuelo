package com.feyconsuelo.infrastructure.service.statistics;

import com.feyconsuelo.application.service.statistiscs.StatisticsAssistEventsService;
import com.feyconsuelo.domain.model.statistics.EventAssistStatisticsResponse;
import com.feyconsuelo.infrastructure.converter.statistics.EventAssistStatisticsToEventAssistStatisticsResponseConverter;
import com.feyconsuelo.infrastructure.repository.StatisticsAssistEventRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

@Slf4j
@Service
@RequiredArgsConstructor
public class StatisticsAssistEventsServiceImpl implements StatisticsAssistEventsService {

    private final StatisticsAssistEventRepository statisticsAssistEventRepository;
    private final EventAssistStatisticsToEventAssistStatisticsResponseConverter eventAssistStatisticsToEventAssistStatisticsResponseConverter;


    @Override
    public EventAssistStatisticsResponse getEventAssistStatistics(
            final LocalDate startDate,
            final LocalDate endDate
    ) {
        final var statistics = this.statisticsAssistEventRepository.getEventsAssistStatistics(startDate, endDate);

        // convert
        return eventAssistStatisticsToEventAssistStatisticsResponseConverter.convert(statistics);
    }

}
