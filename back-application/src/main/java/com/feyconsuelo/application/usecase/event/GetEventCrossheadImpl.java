package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.GetPerformanceCrossheadImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.GetEventCrosshead;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventCrossheadImpl implements GetEventCrosshead {

    private final GetPerformanceCrossheadImpl getPerformanceCrosshead;

    @Override
    public Optional<EventCrosshead> execute(final EventTypeEnum eventType, final Long eventId) {
        if (EventTypeEnum.PERFORMANCE.equals(eventType)) {
            return this.getPerformanceCrosshead.execute(eventId);
        } else {
            throw new FeYConsueloException("No existe cruceta para ensayos");
        }
    }
}
