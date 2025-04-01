package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventMarchStatsResponse;
import com.feyconsuelo.openapi.model.EventMarchStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventMarchStatsResponseToEventMarchStatsResponseDtoConverter {

    public EventMarchStatsResponseDto convert(final EventMarchStatsResponse eventMarchStatsResponse) {
        if (eventMarchStatsResponse == null) {
            return null;
        }
        return EventMarchStatsResponseDto.builder()
                .name(eventMarchStatsResponse.getName())
                .count(eventMarchStatsResponse.getCount())
                .image(eventMarchStatsResponse.getImage())
                .build();
    }

}
