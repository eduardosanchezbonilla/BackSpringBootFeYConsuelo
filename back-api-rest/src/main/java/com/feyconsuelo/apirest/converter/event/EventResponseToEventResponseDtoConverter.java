package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.application.service.utils.DateService;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.openapi.model.EventResponseDto;
import com.feyconsuelo.openapi.model.LatLngResponseDto;
import com.feyconsuelo.openapi.model.VoiceResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.time.format.DateTimeFormatter;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventResponseToEventResponseDtoConverter {

    private static final String TIME_FORMAT = "HH:mm";
    private final DateService dateService;
    private final EventRouteResponseToEventRouteResponseDtoConverter eventRouteResponseToEventRouteResponseDtoConverter;

    private List<VoiceResponseDto> getVoiceList(final EventResponse eventResponse) {
        if (CollectionUtils.isEmpty(eventResponse.getVoiceList())) {
            return List.of();
        } else {
            return eventResponse.getVoiceList().stream()
                    .map(
                            voice -> VoiceResponseDto.builder()
                                    .id(voice.getId())
                                    .name(voice.getName())
                                    .build()
                    )
                    .toList();
        }

    }

    public EventResponseDto convert(final EventResponse eventResponse) {
        return EventResponseDto.builder()
                .id(eventResponse.getId())
                .type(EventResponseDto.TypeEnum.valueOf(eventResponse.getType().name()))
                .title(eventResponse.getTitle())
                .performanceType(eventResponse.getPerformanceType() != null ? EventResponseDto.PerformanceTypeEnum.valueOf(eventResponse.getPerformanceType().name()) : null)
                .description(eventResponse.getDescription())
                .date(eventResponse.getDate())
                .startTime(this.dateService.dateToString(eventResponse.getStartTime(), DateTimeFormatter.ofPattern(TIME_FORMAT)))
                .endTime(this.dateService.dateToString(eventResponse.getEndTime(), DateTimeFormatter.ofPattern(TIME_FORMAT)))
                .voiceIdList(eventResponse.getVoiceIdList())
                .voiceList(this.getVoiceList(eventResponse))
                .repetitionPeriod(eventResponse.getRepetitionPeriod() != null ? EventResponseDto.RepetitionPeriodEnum.valueOf(eventResponse.getRepetitionPeriod().name()) : null)
                .endDate(eventResponse.getEndDate())
                .location(eventResponse.getLocation())
                .province(eventResponse.getProvince())
                .municipality(eventResponse.getMunicipality())
                .image(eventResponse.getImage())
                .clsClass(EventResponseDto.ClsClassEnum.valueOf(eventResponse.getClsClass().name()))
                .displacementBus(eventResponse.getDisplacementBus())
                .musicianBus(eventResponse.getMusicianBus())
                .musicianAssist(eventResponse.getMusicianAssist())
                .route(eventResponse.getRoute() == null ? null : this.eventRouteResponseToEventRouteResponseDtoConverter.convert(eventResponse.getRoute()))
                .currentPosition(
                        eventResponse.getCurrentPosition() == null ?
                                null :
                                LatLngResponseDto.builder()
                                        .lat(eventResponse.getCurrentPosition().getLat())
                                        .lng(eventResponse.getCurrentPosition().getLng())
                                        .build()
                )
                .currentMarch(eventResponse.getCurrentMarch())
                .eventPublic(eventResponse.getEventPublic())
                .repertoirePublic(eventResponse.getRepertoirePublic())
                .crossheadPublic(eventResponse.getCrossheadPublic())
                .busData(eventResponse.getBusData())
                .busTime(this.dateService.dateToString(eventResponse.getBusTime(), DateTimeFormatter.ofPattern(TIME_FORMAT)))
                .busLocation(eventResponse.getBusLocation())
                .duration(eventResponse.getDuration())
                .kilometers(eventResponse.getKilometers())
                .googleId(eventResponse.getGoogleId())
                .build();
    }

}
