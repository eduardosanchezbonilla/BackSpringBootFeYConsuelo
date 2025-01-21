package com.feyconsuelo.application.service.statistiscs;

import com.feyconsuelo.domain.model.statistics.AllMusicianEventAssistStatisticsResponse;
import com.feyconsuelo.domain.model.statistics.MusicianEventAssistStatisticsResponse;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface StatisticsMusicianAssistEventsService {

    Optional<MusicianEventAssistStatisticsResponse> getMusicianEventAssistStatistics(
            Long musicianId,
            LocalDate fromYearDate,
            LocalDate toYearDate,
            LocalDate betweenDatesStart,
            LocalDate betweenDatesEnd
    );

    List<AllMusicianEventAssistStatisticsResponse> getAllMusicianEventAssistStatistics(
            LocalDate startDate,
            LocalDate endDate
    );
}
