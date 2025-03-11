package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;

import java.util.Optional;

public interface GetEventCurrentPosition {

    Optional<LatLng> execute(EventTypeEnum eventType, Long eventId);

}
