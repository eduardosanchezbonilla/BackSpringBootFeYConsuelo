package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.UpdatePerformanceCrossheadImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventCrosshead;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.UpdateEventCrosshead;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateEventCrossheadImpl implements UpdateEventCrosshead {

    private final UpdatePerformanceCrossheadImpl updatePerformanceCrosshead;

    @Override
    public void execute(final EventTypeEnum type, final Long eventId, final EventCrosshead eventCrosshead) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(type)) {
            throw new FeYConsueloException("No se puede actualizar crucetas en ensayos");
        } else {
            this.updatePerformanceCrosshead.update(eventId, eventCrosshead);
        }
    }

}
