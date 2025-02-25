package com.feyconsuelo.application.service.statistiscs;

import com.feyconsuelo.domain.model.statistics.RepertoireMarchEventStatisticResponse;

import java.time.LocalDate;
import java.util.List;

public interface StatisticsRepertoireMarchEventsService {

    List<RepertoireMarchEventStatisticResponse> getRepertoireMarchEventsStatistics(
            LocalDate startDate,
            LocalDate endDate
    );
}
