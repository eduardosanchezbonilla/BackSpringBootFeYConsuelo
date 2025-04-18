package com.feyconsuelo.domain.model.event;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class EventRouteResponse {
    private int zoomLevel;
    private Double rotation;
    private LatLng center;
    private List<LatLng> circles;
    private List<LatLng> route;
    private Double kilometers;
    private List<EventPoiResponse> pois;
}
