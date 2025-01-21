package com.feyconsuelo.application.usecase.statistics.musicianevent;

import com.feyconsuelo.application.service.statistiscs.StatisticsMusicianAssistEventsService;
import com.feyconsuelo.application.usecase.utils.NumberService;
import com.feyconsuelo.domain.model.statistics.AllMusicianEventAssistStatisticsResponse;
import com.feyconsuelo.domain.model.statistics.MusicianEventAssistStatisticsResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class StatisticsMusicianAssistEventsImpl {

    private final NumberService numberService;
    private final StatisticsMusicianAssistEventsService statisticsMusicianAssistEventsService;

    private Double getPercentage(final Integer assists, final Integer total) {
        return total == 0 ? 100.0 : this.numberService.round(((double) assists / total) * 100);
    }

    public MusicianEventAssistStatisticsResponse getPercentageAssistEvents(final Long musicianId, final LocalDate startDate, final LocalDate endDate) {

        final Optional<MusicianEventAssistStatisticsResponse> musicianEventAssistStatisticsResponseOptional = this.statisticsMusicianAssistEventsService.getMusicianEventAssistStatistics(
                musicianId,
                LocalDate.of(startDate.getYear(), 1, 1),
                LocalDate.of(startDate.getYear(), 12, 31),
                startDate,
                endDate
        );

        return musicianEventAssistStatisticsResponseOptional
                .map(
                        musicianEventAssistStatisticsResponse ->
                                MusicianEventAssistStatisticsResponse.builder()
                                        .musicianCurrentMonthPercentageAssistRehearsalEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberRehearsalEvents(), musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberRehearsalEvents()))
                                        .musicianCurrentMonthAssistNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberRehearsalEvents())
                                        .musicianCurrentMonthTotalNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberRehearsalEvents())
                                        .musicianCurrentMonthPercentageAssistPerformanceEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberPerformanceEvents(), musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberPerformanceEvents()))
                                        .musicianCurrentMonthAssistNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberPerformanceEvents())
                                        .musicianCurrentMonthTotalNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberPerformanceEvents())
                                        .musicianCurrentMonthPercentageAssistEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberEvents(), musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberEvents()))
                                        .musicianCurrentMonthAssistNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberEvents())
                                        .musicianCurrentMonthTotalNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberEvents())
                                        .musicianCurrentYearPercentageAssistRehearsalEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberRehearsalEvents(), musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberRehearsalEvents()))
                                        .musicianCurrentYearAssistNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberRehearsalEvents())
                                        .musicianCurrentYearTotalNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberRehearsalEvents())
                                        .musicianCurrentYearPercentageAssistPerformanceEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberPerformanceEvents(), musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberPerformanceEvents()))
                                        .musicianCurrentYearAssistNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberPerformanceEvents())
                                        .musicianCurrentYearTotalNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberPerformanceEvents())
                                        .musicianCurrentYearPercentageAssistEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberEvents(), musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberEvents()))
                                        .musicianCurrentYearAssistNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberEvents())
                                        .musicianCurrentYearTotalNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberEvents())
                                        .musicianHistoricPercentageAssistRehearsalEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberRehearsalEvents(), musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberRehearsalEvents()))
                                        .musicianHistoricAssistNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberRehearsalEvents())
                                        .musicianHistoricTotalNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberRehearsalEvents())
                                        .musicianHistoricPercentageAssistPerformanceEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberPerformanceEvents(), musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberPerformanceEvents()))
                                        .musicianHistoricAssistNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberPerformanceEvents())
                                        .musicianHistoricTotalNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberPerformanceEvents())
                                        .musicianHistoricPercentageAssistEvents(this.getPercentage(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberEvents(), musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberEvents()))
                                        .musicianHistoricAssistNumberEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberEvents())
                                        .musicianHistoricTotalNumberEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberEvents())
                                        .build()
                )
                .orElse(null);

    }

    public List<AllMusicianEventAssistStatisticsResponse> getMusicianAssistInformation(final LocalDate startDate, final LocalDate endDate) {
        return this.statisticsMusicianAssistEventsService.getAllMusicianEventAssistStatistics(startDate, endDate);
    }
}
