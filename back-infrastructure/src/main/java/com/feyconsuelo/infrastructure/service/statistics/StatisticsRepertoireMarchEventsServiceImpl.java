package com.feyconsuelo.infrastructure.service.statistics;

import com.feyconsuelo.application.service.statistiscs.StatisticsRepertoireMarchEventsService;
import com.feyconsuelo.domain.model.statistics.RepertoireMarchEventStatisticResponse;
import com.feyconsuelo.infrastructure.converter.statistics.RepertoireMarchEventStatisticsToRepertoireMarchEventStatisticsResponseConverter;
import com.feyconsuelo.infrastructure.repository.StatisticsRepertoireMarchEventRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class StatisticsRepertoireMarchEventsServiceImpl implements StatisticsRepertoireMarchEventsService {

    private final StatisticsRepertoireMarchEventRepository statisticsRepertoireMarchEventRepository;
    private final RepertoireMarchEventStatisticsToRepertoireMarchEventStatisticsResponseConverter repertoireMarchEventStatisticsToRepertoireMarchEventStatisticsResponseConverter;

    @Override
    public List<RepertoireMarchEventStatisticResponse> getRepertoireMarchEventsStatistics(
            final LocalDate startDate,
            final LocalDate endDate
    ) {
        final var statistics = this.statisticsRepertoireMarchEventRepository.getRepertoireMarchEventStatistics(startDate, endDate);

        // convert
        return this.repertoireMarchEventStatisticsToRepertoireMarchEventStatisticsResponseConverter.convert(statistics);
    }

}
