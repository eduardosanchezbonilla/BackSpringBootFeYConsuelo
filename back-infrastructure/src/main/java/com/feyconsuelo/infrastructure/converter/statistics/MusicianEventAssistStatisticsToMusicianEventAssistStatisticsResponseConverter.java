package com.feyconsuelo.infrastructure.converter.statistics;

import com.feyconsuelo.domain.model.statistics.MusicianEventAssistStatisticsResponse;
import com.feyconsuelo.infrastructure.entities.statistics.MusicianEventAssistStatistics;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEventAssistStatisticsToMusicianEventAssistStatisticsResponseConverter {

    public MusicianEventAssistStatisticsResponse convert(final MusicianEventAssistStatistics entity) {
        return MusicianEventAssistStatisticsResponse.builder()
                .musicianCurrentMonthAssistNumberRehearsalEvents(entity.getMusicianBetweenDatesAssistNumberRehearsalEvents())
                .musicianCurrentMonthTotalNumberRehearsalEvents(entity.getMusicianBetweenDatesTotalNumberRehearsalEvents())
                .musicianCurrentMonthAssistNumberPerformanceEvents(entity.getMusicianBetweenDatesAssistNumberPerformanceEvents())
                .musicianCurrentMonthTotalNumberPerformanceEvents(entity.getMusicianBetweenDatesTotalNumberPerformanceEvents())
                .musicianCurrentMonthAssistNumberEvents(entity.getMusicianBetweenDatesAssistNumberEvents())
                .musicianCurrentMonthTotalNumberEvents(entity.getMusicianBetweenDatesTotalNumberEvents())
                .musicianCurrentYearAssistNumberRehearsalEvents(entity.getMusicianFromDateAssistNumberRehearsalEvents())
                .musicianCurrentYearTotalNumberRehearsalEvents(entity.getMusicianFromDateTotalNumberRehearsalEvents())
                .musicianCurrentYearAssistNumberPerformanceEvents(entity.getMusicianFromDateAssistNumberPerformanceEvents())
                .musicianCurrentYearTotalNumberPerformanceEvents(entity.getMusicianFromDateTotalNumberPerformanceEvents())
                .musicianCurrentYearAssistNumberEvents(entity.getMusicianFromDateAssistNumberEvents())
                .musicianCurrentYearTotalNumberEvents(entity.getMusicianFromDateTotalNumberEvents())
                .musicianHistoricAssistNumberRehearsalEvents(entity.getMusicianHistoricAssistNumberRehearsalEvents())
                .musicianHistoricTotalNumberRehearsalEvents(entity.getMusicianHistoricTotalNumberRehearsalEvents())
                .musicianHistoricAssistNumberPerformanceEvents(entity.getMusicianHistoricAssistNumberPerformanceEvents())
                .musicianHistoricTotalNumberPerformanceEvents(entity.getMusicianHistoricTotalNumberPerformanceEvents())
                .musicianHistoricAssistNumberEvents(entity.getMusicianHistoricAssistNumberEvents())
                .musicianHistoricTotalNumberEvents(entity.getMusicianHistoricTotalNumberEvents())
                .build();
    }

}
