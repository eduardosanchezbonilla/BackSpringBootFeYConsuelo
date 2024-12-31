package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.apirest.converter.voice.VoiceResponseListToVoiceResponseDtoListConverter;
import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.event.EventPerformanceTypeEnum;
import com.feyconsuelo.domain.model.event.EventRepetitionPeriodEnum;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.openapi.model.EventRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.format.DateTimeFormatter;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventRequestDtoToEventRequestConverter {

    private final DateService dateService;
    private final VoiceResponseListToVoiceResponseDtoListConverter voiceResponseListToVoiceResponseDtoListConverter;

    @Value("${default-images.event}")
    private String defaultEventImage;

    private String getEventImage(final String image) {
        if (StringUtils.isEmpty(image)) {
            return null;
        } else {
            if (image.equals(this.defaultEventImage)) {
                return null;
            } else {
                return image;
            }
        }
    }

    public EventRequest convert(final EventRequestDto eventRequestDto) {
        return EventRequest.builder()
                .type(eventRequestDto.getType() != null ? EventTypeEnum.valueOf(eventRequestDto.getType().getValue()) : null)
                .title(eventRequestDto.getTitle())
                .description(eventRequestDto.getDescription())
                .performanceType(eventRequestDto.getPerformanceType() != null ? EventPerformanceTypeEnum.valueOf(eventRequestDto.getPerformanceType().getValue()) : null)
                .date(eventRequestDto.getDate())
                .startTime(this.dateService.localDateAndStringTimeToLocalDateTime(eventRequestDto.getDate(), eventRequestDto.getStartTime(), DateTimeFormatter.ISO_DATE_TIME))
                .endTime(this.dateService.localDateAndStringTimeToLocalDateTime(eventRequestDto.getDate(), eventRequestDto.getEndTime(), DateTimeFormatter.ISO_DATE_TIME))
                .voiceIdList(eventRequestDto.getVoiceIdList())
                .repetitionPeriod(eventRequestDto.getRepetitionPeriod() != null ? EventRepetitionPeriodEnum.valueOf(eventRequestDto.getRepetitionPeriod().getValue()) : null)
                .endDate(eventRequestDto.getEndDate())
                .location(eventRequestDto.getLocation())
                .municipality(eventRequestDto.getMunicipality())
                .province(eventRequestDto.getProvince())
                .image(this.getEventImage(eventRequestDto.getImage()))
                .displacementBus(eventRequestDto.getDisplacementBus())
                .build();
    }

}
