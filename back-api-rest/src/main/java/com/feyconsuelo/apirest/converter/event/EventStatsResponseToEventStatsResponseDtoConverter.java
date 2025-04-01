package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventStatsResponse;
import com.feyconsuelo.openapi.model.EventStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventStatsResponseToEventStatsResponseDtoConverter {

    private final EventDetailStatsResponseToEventDetailStatsResponseDtoConverter eventDetailStatsResponseToEventDetailStatsResponseDtoConverter;
    private final EventStatInfoResponseToEventStatInfoResponseDtoConverter eventStatInfoResponseToEventStatInfoResponseDtoConverter;

    public EventStatsResponseDto convert(final EventStatsResponse eventStatsResponse) {
        return EventStatsResponseDto.builder()
                .event(eventStatsResponse.getEvent() != null ? this.eventStatInfoResponseToEventStatInfoResponseDtoConverter.convert(eventStatsResponse.getEvent()) : null)
                .stats(eventStatsResponse.getStats() != null ? this.eventDetailStatsResponseToEventDetailStatsResponseDtoConverter.convert(eventStatsResponse.getStats()) : null)
                .build();

    }

}
