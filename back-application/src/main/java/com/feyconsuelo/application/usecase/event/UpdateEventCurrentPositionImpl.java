package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.UpdatePerformanceCurrentPositionImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.event.LatLng;
import com.feyconsuelo.domain.usecase.event.UpdateEventCurrentPosition;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateEventCurrentPositionImpl implements UpdateEventCurrentPosition {

    private final UpdatePerformanceCurrentPositionImpl updatePerformanceCurrentPosition;

    @Override
    public void execute(final EventTypeEnum type, final Long eventId, final LatLng latLng) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(type)) {
            throw new FeYConsueloException("No se puede actualizar la posicion actual en ensayos");
        } else {
            this.updatePerformanceCurrentPosition.update(eventId, latLng);
        }
    }

}
