package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.GetPerformanceCurrentPositionImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.usecase.event.GetEventCurrentPosition;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventCurrentPositionImpl implements GetEventCurrentPosition {

    private final GetPerformanceCurrentPositionImpl getPerformanceCurrentPosition;

    @Override
    public Optional<LatLng> execute(final EventTypeEnum eventType, final Long eventId) {
        if (EventTypeEnum.PERFORMANCE.equals(eventType)) {
            return this.getPerformanceCurrentPosition.execute(eventId);
        } else {
            throw new FeYConsueloException("No existe la posicion actual para ensayos");
        }
    }
}
