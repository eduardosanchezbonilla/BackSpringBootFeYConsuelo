package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.openapi.model.EventGroupByAnyoResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventResponseListToEventGroupByAnyoResponseDtoListConverter {

    private final EventResponseToEventResponseDtoConverter eventResponseToEventResponseDtoConverter;

    public List<EventGroupByAnyoResponseDto> convert(final List<EventResponse> eventResponseList, final String filterName) {
        if (CollectionUtils.isEmpty(eventResponseList)) {
            return List.of();
        }

        // tenemos el array de eventos, y lo primero que tenemos que hacer es filtrarlo, y despues agrupar por anyo, y devolver para cada anyo todos los eventos
        return eventResponseList.stream()
                .filter(
                        eventResponse ->
                                (StringUtils.isEmpty(filterName)
                                        ||
                                        (StringUtils.isNotEmpty(eventResponse.getTitle()) && eventResponse.getTitle().toUpperCase().contains(filterName.toUpperCase()))
                                        ||
                                        (StringUtils.isNotEmpty(eventResponse.getDescription()) && eventResponse.getDescription().toUpperCase().contains(filterName.toUpperCase()))
                                )
                )
                .collect(Collectors.groupingBy(eventResponse -> eventResponse.getDate().getYear()))
                .entrySet().stream()
                .map(
                        entry -> EventGroupByAnyoResponseDto.builder()
                                .anyo(entry.getKey() + "")
                                .events(
                                        entry.getValue().stream()
                                                .map(this.eventResponseToEventResponseDtoConverter::convert)
                                                .toList()
                                )
                                .build()
                )
                .toList();

    }

}
