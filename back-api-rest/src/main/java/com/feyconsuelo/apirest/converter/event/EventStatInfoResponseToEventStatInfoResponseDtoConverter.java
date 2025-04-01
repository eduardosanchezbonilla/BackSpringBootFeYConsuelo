package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.event.EventStatInfoResponse;
import com.feyconsuelo.openapi.model.EventStatInfoResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventStatInfoResponseToEventStatInfoResponseDtoConverter {

    private static final String TIME_FORMAT = "HH:mm";
    private final DateService dateService;

    public EventStatInfoResponseDto convert(final EventStatInfoResponse eventStatInfoResponse) {
        return EventStatInfoResponseDto.builder()
                .id(eventStatInfoResponse.getId())
                .type(EventStatInfoResponseDto.TypeEnum.valueOf(eventStatInfoResponse.getType().toString()))
                .title(eventStatInfoResponse.getTitle())
                .performanceType(eventStatInfoResponse.getPerformanceType() != null ? EventStatInfoResponseDto.PerformanceTypeEnum.valueOf(eventStatInfoResponse.getPerformanceType().toString()) : null)
                .description(eventStatInfoResponse.getDescription())
                .date(eventStatInfoResponse.getDate())
                .startTime(this.dateService.dateToString(eventStatInfoResponse.getStartTime(), DateTimeFormatter.ofPattern(TIME_FORMAT)))
                .endTime(this.dateService.dateToString(eventStatInfoResponse.getEndTime(), DateTimeFormatter.ofPattern(TIME_FORMAT)))
                .province(eventStatInfoResponse.getProvince())
                .municipality(eventStatInfoResponse.getMunicipality())
                .image(eventStatInfoResponse.getImage())
                .duration(eventStatInfoResponse.getDuration())
                .kilometers(eventStatInfoResponse.getKilometers())
                .build();
    }

}
