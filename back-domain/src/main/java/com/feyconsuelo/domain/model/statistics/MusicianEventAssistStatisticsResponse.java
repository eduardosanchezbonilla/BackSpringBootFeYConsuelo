package com.feyconsuelo.domain.model.statistics;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class MusicianEventAssistStatisticsResponse {
    private Double musicianCurrentMonthPercentageAssistRehearsalEvents;
    private Integer musicianCurrentMonthAssistNumberRehearsalEvents;
    private Integer musicianCurrentMonthTotalNumberRehearsalEvents;
    private Double musicianCurrentMonthPercentageAssistPerformanceEvents;
    private Integer musicianCurrentMonthAssistNumberPerformanceEvents;
    private Integer musicianCurrentMonthTotalNumberPerformanceEvents;
    private Double musicianCurrentMonthPercentageAssistEvents;
    private Integer musicianCurrentMonthAssistNumberEvents;
    private Integer musicianCurrentMonthTotalNumberEvents;
    private Double musicianCurrentYearPercentageAssistRehearsalEvents;
    private Integer musicianCurrentYearAssistNumberRehearsalEvents;
    private Integer musicianCurrentYearTotalNumberRehearsalEvents;
    private Double musicianCurrentYearPercentageAssistPerformanceEvents;
    private Integer musicianCurrentYearAssistNumberPerformanceEvents;
    private Integer musicianCurrentYearTotalNumberPerformanceEvents;
    private Double musicianCurrentYearPercentageAssistEvents;
    private Integer musicianCurrentYearAssistNumberEvents;
    private Integer musicianCurrentYearTotalNumberEvents;
    private Double musicianHistoricPercentageAssistRehearsalEvents;
    private Integer musicianHistoricAssistNumberRehearsalEvents;
    private Integer musicianHistoricTotalNumberRehearsalEvents;
    private Double musicianHistoricPercentageAssistPerformanceEvents;
    private Integer musicianHistoricAssistNumberPerformanceEvents;
    private Integer musicianHistoricTotalNumberPerformanceEvents;
    private Double musicianHistoricPercentageAssistEvents;
    private Integer musicianHistoricAssistNumberEvents;
    private Integer musicianHistoricTotalNumberEvents;
}
