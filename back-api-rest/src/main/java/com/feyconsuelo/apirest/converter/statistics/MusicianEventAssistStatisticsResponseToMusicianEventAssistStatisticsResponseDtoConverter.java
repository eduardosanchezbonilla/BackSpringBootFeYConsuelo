package com.feyconsuelo.apirest.converter.statistics;

import com.feyconsuelo.domain.model.statistics.MusicianEventAssistStatisticsResponse;
import com.feyconsuelo.openapi.model.MusicianEventAssistStatisticsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class MusicianEventAssistStatisticsResponseToMusicianEventAssistStatisticsResponseDtoConverter {

    public MusicianEventAssistStatisticsResponseDto convert(final MusicianEventAssistStatisticsResponse musicianEventAssistStatisticsResponse) {
        if (musicianEventAssistStatisticsResponse == null) {
            return null;
        }
        return MusicianEventAssistStatisticsResponseDto.builder()
                .musicianCurrentMonthPercentageAssistRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthPercentageAssistRehearsalEvents())
                .musicianCurrentMonthAssistNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberRehearsalEvents())
                .musicianCurrentMonthTotalNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberRehearsalEvents())
                .musicianCurrentMonthPercentageAssistPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthPercentageAssistPerformanceEvents())
                .musicianCurrentMonthAssistNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberPerformanceEvents())
                .musicianCurrentMonthTotalNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberPerformanceEvents())
                .musicianCurrentMonthPercentageAssistEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthPercentageAssistEvents())
                .musicianCurrentMonthAssistNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthAssistNumberEvents())
                .musicianCurrentMonthTotalNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentMonthTotalNumberEvents())
                .musicianCurrentYearPercentageAssistRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearPercentageAssistRehearsalEvents())
                .musicianCurrentYearAssistNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberRehearsalEvents())
                .musicianCurrentYearTotalNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberRehearsalEvents())
                .musicianCurrentYearPercentageAssistPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearPercentageAssistPerformanceEvents())
                .musicianCurrentYearAssistNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberPerformanceEvents())
                .musicianCurrentYearTotalNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberPerformanceEvents())
                .musicianCurrentYearPercentageAssistEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearPercentageAssistEvents())
                .musicianCurrentYearAssistNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearAssistNumberEvents())
                .musicianCurrentYearTotalNumberEvents(musicianEventAssistStatisticsResponse.getMusicianCurrentYearTotalNumberEvents())
                .musicianHistoricPercentageAssistRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricPercentageAssistRehearsalEvents())
                .musicianHistoricAssistNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberRehearsalEvents())
                .musicianHistoricTotalNumberRehearsalEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberRehearsalEvents())
                .musicianHistoricPercentageAssistPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricPercentageAssistPerformanceEvents())
                .musicianHistoricAssistNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberPerformanceEvents())
                .musicianHistoricTotalNumberPerformanceEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberPerformanceEvents())
                .musicianHistoricPercentageAssistEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricPercentageAssistEvents())
                .musicianHistoricAssistNumberEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricAssistNumberEvents())
                .musicianHistoricTotalNumberEvents(musicianEventAssistStatisticsResponse.getMusicianHistoricTotalNumberEvents())
                .build();
    }

}
