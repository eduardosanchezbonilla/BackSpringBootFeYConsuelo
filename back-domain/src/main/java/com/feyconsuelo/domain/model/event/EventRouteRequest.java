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
public class EventRouteRequest {

    private LatLng center;
    private int zoomLevel;
    private Double rotation;
    private List<LatLng> route;
    private List<LatLng> circles;
    private Double kilometers;
    private List<EventPoiRequest> pois;

}
