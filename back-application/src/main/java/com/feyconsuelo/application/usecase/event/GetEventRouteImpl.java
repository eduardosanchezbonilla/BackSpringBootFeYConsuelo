package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.GetPerformanceRouteImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventRouteResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.GetEventRoute;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventRouteImpl implements GetEventRoute {

    private final GetPerformanceRouteImpl getPerformanceRoute;

    @Override
    public Optional<EventRouteResponse> execute(final EventTypeEnum eventType, final Long eventId) {
        if (EventTypeEnum.PERFORMANCE.equals(eventType)) {
            return this.getPerformanceRoute.execute(eventId);
        } else {
            throw new FeYConsueloException("No existe ruta para ensayos");
        }
    }
}
