package com.feyconsuelo.infrastructure.service.statistics;

import com.feyconsuelo.application.service.statistiscs.StatisticsMusicianAssistEventsService;
import com.feyconsuelo.domain.model.statistics.AllMusicianEventAssistStatisticsResponse;
import com.feyconsuelo.domain.model.statistics.MusicianEventAssistStatisticsResponse;
import com.feyconsuelo.infrastructure.converter.statistics.AllMusicianEventAssistStatisticsToAllMusicianEventAssistStatisticsResponseConverter;
import com.feyconsuelo.infrastructure.converter.statistics.MusicianEventAssistStatisticsToMusicianEventAssistStatisticsResponseConverter;
import com.feyconsuelo.infrastructure.repository.StatisticsMusicianAssistEventRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class StatisticsMusicianAssistEventsServiceImpl implements StatisticsMusicianAssistEventsService {

    private final StatisticsMusicianAssistEventRepository statisticsMusicianAssistEventRepository;
    private final MusicianEventAssistStatisticsToMusicianEventAssistStatisticsResponseConverter musicianEventAssistStatisticsToMusicianEventAssistStatisticsResponseConverter;
    private final AllMusicianEventAssistStatisticsToAllMusicianEventAssistStatisticsResponseConverter allMusicianEventAssistStatisticsToAllMusicianEventAssistStatisticsResponseConverter;


    @Override
    public Optional<MusicianEventAssistStatisticsResponse> getMusicianEventAssistStatistics(
            final Long musicianId,
            final LocalDate fromYearDate,
            final LocalDate toYearDate,
            final LocalDate betweenDatesStart,
            final LocalDate betweenDatesEnd
    ) {
        final var musician = this.statisticsMusicianAssistEventRepository.getMusicianEventAssistStatistics(musicianId, fromYearDate, toYearDate, betweenDatesStart, betweenDatesEnd);

        // convert
        return musician.map(this.musicianEventAssistStatisticsToMusicianEventAssistStatisticsResponseConverter::convert);
    }

    @Override
    public List<AllMusicianEventAssistStatisticsResponse> getAllMusicianEventAssistStatistics(
            final LocalDate startDate,
            final LocalDate endDate
    ) {
        final var statistics = this.statisticsMusicianAssistEventRepository.getAllMusicianEventAssistStatistics(startDate, endDate);

        // convert
        return this.allMusicianEventAssistStatisticsToAllMusicianEventAssistStatisticsResponseConverter.convert(statistics);
    }

}
