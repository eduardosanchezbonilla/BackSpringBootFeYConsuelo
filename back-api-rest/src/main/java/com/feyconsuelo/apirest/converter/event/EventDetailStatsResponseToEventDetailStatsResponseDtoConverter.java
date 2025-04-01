package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventDetailStatsResponse;
import com.feyconsuelo.openapi.model.EventDetailStatsResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventDetailStatsResponseToEventDetailStatsResponseDtoConverter {

    private final EventMarchStatsResponseToEventMarchStatsResponseDtoConverter eventMarchStatsResponseToEventMarchStatsResponseDtoConverter;
    private final EventMarchTypeStatsResponseToEventMarchTypeStatsResponseDtoConverter eventMarchTypeStatsResponseToEventMarchTypeStatsResponseDtoConverter;

    public EventDetailStatsResponseDto convert(final EventDetailStatsResponse eventDetailStatsResponse) {
        return EventDetailStatsResponseDto.builder()
                .marchsStats(
                        CollectionUtils.isEmpty(eventDetailStatsResponse.getMarchsStats()) ?
                                List.of() :
                                eventDetailStatsResponse.getMarchsStats().stream()
                                        .map(this.eventMarchStatsResponseToEventMarchStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .marchsTypeStats(
                        CollectionUtils.isEmpty(eventDetailStatsResponse.getMarchsTypeStats()) ?
                                List.of() :
                                eventDetailStatsResponse.getMarchsTypeStats().stream()
                                        .map(this.eventMarchTypeStatsResponseToEventMarchTypeStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .mostPlayerMarchOwnSlow(
                        CollectionUtils.isEmpty(eventDetailStatsResponse.getMostPlayerMarchOwnSlow()) ?
                                List.of() :
                                eventDetailStatsResponse.getMostPlayerMarchOwnSlow().stream()
                                        .map(this.eventMarchStatsResponseToEventMarchStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .leastPlayerMarchOwnSlow(
                        CollectionUtils.isEmpty(eventDetailStatsResponse.getLeastPlayerMarchOwnSlow()) ?
                                List.of() :
                                eventDetailStatsResponse.getLeastPlayerMarchOwnSlow().stream()
                                        .map(this.eventMarchStatsResponseToEventMarchStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .mostPlayerMarchOtherSlow(
                        CollectionUtils.isEmpty(eventDetailStatsResponse.getMostPlayerMarchOtherSlow()) ?
                                List.of() :
                                eventDetailStatsResponse.getMostPlayerMarchOtherSlow().stream()
                                        .map(this.eventMarchStatsResponseToEventMarchStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .leastPlayerMarchOtherSlow(
                        CollectionUtils.isEmpty(eventDetailStatsResponse.getLeastPlayerMarchOtherSlow()) ?
                                List.of() :
                                eventDetailStatsResponse.getLeastPlayerMarchOtherSlow().stream()
                                        .map(this.eventMarchStatsResponseToEventMarchStatsResponseDtoConverter::convert)
                                        .toList()
                )
                .countMostPlayerMarch(eventDetailStatsResponse.getCountMostPlayerMarch())
                .totalNumberMarchs(eventDetailStatsResponse.getTotalNumberMarchs())
                .totalNumberMarchsOwnSlow(eventDetailStatsResponse.getTotalNumberMarchsOwnSlow())
                .totalNumberMarchsOtherSlow(eventDetailStatsResponse.getTotalNumberMarchsOtherSlow())
                .totalNumberMarchsOrdinary(eventDetailStatsResponse.getTotalNumberMarchsOrdinary())
                .totalNumberHours(eventDetailStatsResponse.getTotalNumberHours())
                .totalNumberKilometers(eventDetailStatsResponse.getTotalNumberKilometers())
                .averageNumberMarchsByHour(eventDetailStatsResponse.getAverageNumberMarchsByHour())
                .percentageNumberMarchsOwn(eventDetailStatsResponse.getPercentageNumberMarchsOwn())
                .percentageNumberMarchsOther(eventDetailStatsResponse.getPercentageNumberMarchsOther())
                .build();
    }

}
