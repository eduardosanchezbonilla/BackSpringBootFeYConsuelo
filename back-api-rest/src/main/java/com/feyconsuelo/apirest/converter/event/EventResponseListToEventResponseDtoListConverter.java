package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.openapi.model.EventResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventResponseListToEventResponseDtoListConverter {

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;

    public List<EventResponseDto> convert(final List<EventResponse> eventResponseList) {
        if (CollectionUtils.isEmpty(eventResponseList)) {
            return List.of();
        }
        return eventResponseList.stream()
                .map(this.eventResponseToEventResponseDtoConverter::convert)
                .toList();
    }

}
