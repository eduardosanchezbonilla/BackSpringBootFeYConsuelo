package com.feyconsuelo.infrastructure.converter.statistics;

import com.feyconsuelo.domain.model.statistics.EventAssistStatisticsResponse;
import com.feyconsuelo.infrastructure.entities.statistics.EventAssistStatistics;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventAssistStatisticsToEventAssistStatisticsResponseConverter {

    public EventAssistStatisticsResponse convert(final EventAssistStatistics entity) {
        return EventAssistStatisticsResponse.builder()
                .averageAssitsNumber(entity.getAverageAssitsNumber())
                .maxAssitsNumber(entity.getMaxAssitsNumber())
                .maxDateAssitsNumber(entity.getDateMaxAssitsNumber())
                .minAssitsNumber(entity.getMinAssitsNumber())
                .minDateAssitsNumber(entity.getDateMinAssitsNumber())
                .build();
    }

}
