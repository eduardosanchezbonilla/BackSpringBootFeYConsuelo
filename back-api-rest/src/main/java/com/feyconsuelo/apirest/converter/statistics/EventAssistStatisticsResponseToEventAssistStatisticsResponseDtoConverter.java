package com.feyconsuelo.apirest.converter.statistics;

import com.feyconsuelo.domain.model.statistics.EventAssistStatisticsResponse;
import com.feyconsuelo.openapi.model.EventAssistStatisticsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventAssistStatisticsResponseToEventAssistStatisticsResponseDtoConverter {

    public EventAssistStatisticsResponseDto convert(final EventAssistStatisticsResponse eventAssistStatisticsResponse) {
        if (eventAssistStatisticsResponse == null) {
            return null;
        }
        return EventAssistStatisticsResponseDto.builder()
                .averageAssitsNumber(eventAssistStatisticsResponse.getAverageAssitsNumber())
                .maxAssitsNumber(eventAssistStatisticsResponse.getMaxAssitsNumber())
                .maxDateAssitsNumber(eventAssistStatisticsResponse.getMaxDateAssitsNumber())
                .minAssitsNumber(eventAssistStatisticsResponse.getMinAssitsNumber())
                .minDateAssitsNumber(eventAssistStatisticsResponse.getMinDateAssitsNumber())
                .build();
    }

}
