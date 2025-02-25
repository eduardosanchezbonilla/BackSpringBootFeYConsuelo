package com.feyconsuelo.domain.model.musicianevent;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.statistics.AllMusicianEventAssistStatisticsResponse;
import com.feyconsuelo.domain.model.statistics.EventAssistStatisticsResponse;
import com.feyconsuelo.domain.model.statistics.MusicianEventAssistStatisticsResponse;
import com.feyconsuelo.domain.model.statistics.RepertoireMarchEventStatisticResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class MusicianEventListResponse {
    private MusicianEventAssistStatisticsResponse musicianEventAssistStatistics;
    private List<AllMusicianEventAssistStatisticsResponse> musicianAssitsInformation;
    private EventAssistStatisticsResponse eventAssistStatisticsResponse;
    private List<RepertoireMarchEventStatisticResponse> repertoireMarchEventStatistic;
    private List<EventResponse> events;
}
