package com.feyconsuelo.application.usecase.statistics.repertoiremarch;

import com.feyconsuelo.application.service.statistiscs.StatisticsRepertoireMarchEventsService;
import com.feyconsuelo.domain.model.statistics.RepertoireMarchEventStatisticResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;

@Component
@RequiredArgsConstructor
public class StatisticsRepertoireMarchEventsImpl {

    private final StatisticsRepertoireMarchEventsService statisticsRepertoireMarchEventsService;


    public List<RepertoireMarchEventStatisticResponse> getRepertoireMarchEventStatistic(final LocalDate startDate, final LocalDate endDate) {
        return this.statisticsRepertoireMarchEventsService.getRepertoireMarchEventsStatistics(startDate, endDate);
    }
}
