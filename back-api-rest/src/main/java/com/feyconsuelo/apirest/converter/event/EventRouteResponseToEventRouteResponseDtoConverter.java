package com.feyconsuelo.apirest.converter.event;

import com.feyconsuelo.domain.model.event.EventRouteResponse;
import com.feyconsuelo.openapi.model.EventRouteResponseDto;
import com.feyconsuelo.openapi.model.LatLngRequestDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

@Slf4j
@Component
@RequiredArgsConstructor
public class EventRouteResponseToEventRouteResponseDtoConverter {

    private final EventPoiResponseDtoToEventPoiResponseConverter eventPoiResponseDtoToEventPoiResponseConverter;

    public EventRouteResponseDto convert(final EventRouteResponse eventRouteResponse) {
        return EventRouteResponseDto.builder()
                .zoomLevel(eventRouteResponse.getZoomLevel() == 0 ? 17 : eventRouteResponse.getZoomLevel())
                .rotation(eventRouteResponse.getRotation() == null ? 0.0 : eventRouteResponse.getRotation())
                .center(eventRouteResponse.getCenter() == null ?
                        LatLngRequestDto.builder()
                                .lat(37.7191055)
                                .lng(-3.9737003)
                                .build() :
                        LatLngRequestDto.builder()
                                .lat(eventRouteResponse.getCenter().getLat())
                                .lng(eventRouteResponse.getCenter().getLng())
                                .build()
                )
                .route(
                        CollectionUtils.isEmpty(eventRouteResponse.getRoute()) ?
                                null :
                                eventRouteResponse.getRoute().stream()
                                        .map(
                                                latLng -> LatLngRequestDto.builder()
                                                        .lat(latLng.getLat())
                                                        .lng(latLng.getLng())
                                                        .build()
                                        )
                                        .toList()
                )
                .circles(
                        CollectionUtils.isEmpty(eventRouteResponse.getCircles()) ?
                                null :
                                eventRouteResponse.getCircles().stream()
                                        .map(
                                                circle -> LatLngRequestDto.builder()
                                                        .lat(circle.getLat())
                                                        .lng(circle.getLng())
                                                        .build()
                                        )
                                        .toList()
                )
                .kilometers(eventRouteResponse.getKilometers())
                .pois(
                        CollectionUtils.isEmpty(eventRouteResponse.getPois()) ?
                                null :
                                eventRouteResponse.getPois().stream()
                                        .map(eventPoiResponseDtoToEventPoiResponseConverter::convert)
                                        .toList()
                )
                .build();
    }

}
