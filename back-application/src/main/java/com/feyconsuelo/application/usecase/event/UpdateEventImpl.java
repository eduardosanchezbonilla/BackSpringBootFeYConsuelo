package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.UpdatePerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.UpdateRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.UpdateEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateEventImpl implements UpdateEvent {

    private final UpdatePerformanceImpl updatePerformance;
    private final UpdateRehearsalImpl updateRehearsal;

    @Override
    public void execute(final EventTypeEnum type, final Long eventId, final EventRequest eventRequest) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(eventRequest.getType())) {
            this.updateRehearsal.update(eventId, eventRequest);
        } else {
            this.updatePerformance.update(eventId, eventRequest);
        }
    }

}
