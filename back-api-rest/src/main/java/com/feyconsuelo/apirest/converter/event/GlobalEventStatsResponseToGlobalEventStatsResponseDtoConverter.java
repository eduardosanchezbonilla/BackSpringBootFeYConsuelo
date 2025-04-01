package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.GlobalEventStatsResponse;
import com.feyconsuelo.openapi.model.GlobalEventStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class GlobalEventStatsResponseToGlobalEventStatsResponseDtoConverter {

    private final EventDetailStatsResponseToEventDetailStatsResponseDtoConverter eventDetailStatsResponseToEventDetailStatsResponseDtoConverter;
    private final EventStatsResponseToEventStatsResponseDtoConverter eventStatsResponseToEventStatsResponseDtoConverter;

    public GlobalEventStatsResponseDto convert(final GlobalEventStatsResponse globalEventStatsResponse) {
        return GlobalEventStatsResponseDto.builder()
                .totalStats(
                        globalEventStatsResponse.getTotalStats() != null ?
                                this.eventDetailStatsResponseToEventDetailStatsResponseDtoConverter.convert(globalEventStatsResponse.getTotalStats()) :
                                null
                )
                .eventStats(
                        CollectionUtils.isEmpty(globalEventStatsResponse.getEventStats()) ?
                                List.of() :
                                globalEventStatsResponse.getEventStats().stream()
                                        .map(this.eventStatsResponseToEventStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .build();

    }
}
