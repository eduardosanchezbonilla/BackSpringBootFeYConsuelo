package com.feyconsuelo.domain.usecase.event;

import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;

public interface UpdateEventCurrentPosition {

    void execute(EventTypeEnum eventType, Long eventId, LatLng latLng);

}
