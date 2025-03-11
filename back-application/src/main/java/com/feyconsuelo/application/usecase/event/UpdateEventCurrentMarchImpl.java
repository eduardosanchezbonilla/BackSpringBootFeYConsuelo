package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.UpdatePerformanceCurrentMarchImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.UpdateEventCurrentMarch;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateEventCurrentMarchImpl implements UpdateEventCurrentMarch {

    private final UpdatePerformanceCurrentMarchImpl updatePerformanceCurrentMarch;

    @Override
    public void execute(final EventTypeEnum type, final Long eventId, final String march) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(type)) {
            throw new FeYConsueloException("No se puede actualizar la posicion actual en ensayos");
        } else {
            this.updatePerformanceCurrentMarch.update(eventId, march);
        }
    }

}
