package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.UpdatePerformanceRouteImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventRouteRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.UpdateEventRoute;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateEventRouteImpl implements UpdateEventRoute {

    private final UpdatePerformanceRouteImpl updatePerformanceRoute;

    @Override
    public void execute(final EventTypeEnum type, final Long eventId, final EventRouteRequest eventRouteRequest) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(type)) {
            throw new FeYConsueloException("No se puede actualizar rutas en ensayos");
        } else {
            this.updatePerformanceRoute.update(eventId, eventRouteRequest);
        }
    }

}
