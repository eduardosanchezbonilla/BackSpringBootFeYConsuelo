package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventRouteRequest;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.openapi.model.EventRouteRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventRouteRequestDtoToEventRouteRequestConverter {

    public EventRouteRequest convert(final EventRouteRequestDto eventRouteRequestDto) {
        return EventRouteRequest.builder()
                .zoomLevel(eventRouteRequestDto.getZoomLevel())
                .rotation(eventRouteRequestDto.getRotation() == null ? 0.0 : eventRouteRequestDto.getRotation())
                .center(
                        LatLng.builder()
                                .lat(eventRouteRequestDto.getCenter().getLat())
                                .lng(eventRouteRequestDto.getCenter().getLng())
                                .build()
                )
                .route(
                        CollectionUtils.isEmpty(eventRouteRequestDto.getRoute()) ?
                                null :
                                eventRouteRequestDto.getRoute().stream()
                                        .map(
                                                latLng -> LatLng.builder()
                                                        .lat(latLng.getLat())
                                                        .lng(latLng.getLng())
                                                        .build()
                                        )
                                        .toList()
                )
                .circles(
                        CollectionUtils.isEmpty(eventRouteRequestDto.getCircles()) ?
                                null :
                                eventRouteRequestDto.getCircles().stream()
                                        .map(
                                                circle -> LatLng.builder()
                                                        .lat(circle.getLat())
                                                        .lng(circle.getLng())
                                                        .build()
                                        )
                                        .toList()
                )
                .kilometers(eventRouteRequestDto.getKilometers())
                .build();
    }

}
