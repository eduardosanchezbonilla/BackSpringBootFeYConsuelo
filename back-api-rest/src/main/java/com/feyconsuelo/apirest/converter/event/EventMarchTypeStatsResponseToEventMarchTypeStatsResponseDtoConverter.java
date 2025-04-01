package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventMarchTypeStatsResponse;
import com.feyconsuelo.openapi.model.EventMarchTypeStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventMarchTypeStatsResponseToEventMarchTypeStatsResponseDtoConverter {

    private final EventMarchStatsResponseToEventMarchStatsResponseDtoConverter eventMarchStatsResponseToEventMarchStatsResponseDtoConverter;

    public EventMarchTypeStatsResponseDto convert(final EventMarchTypeStatsResponse eventMarchTypeStatsResponse) {
        if (eventMarchTypeStatsResponse == null) {
            return null;
        }
        return EventMarchTypeStatsResponseDto.builder()
                .name(eventMarchTypeStatsResponse.getName())
                .count(eventMarchTypeStatsResponse.getCount())
                .image(eventMarchTypeStatsResponse.getImage())
                .mostPlayerMarch(
                        CollectionUtils.isEmpty(eventMarchTypeStatsResponse.getMostPlayerMarch()) ?
                                List.of() :
                                eventMarchTypeStatsResponse.getMostPlayerMarch().stream()
                                        .map(this.eventMarchStatsResponseToEventMarchStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .leastPlayerMarch(
                        CollectionUtils.isEmpty(eventMarchTypeStatsResponse.getLeastPlayerMarch()) ?
                                List.of() :
                                eventMarchTypeStatsResponse.getLeastPlayerMarch().stream()
                                        .map(this.eventMarchStatsResponseToEventMarchStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .build();
    }

}
