package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.InsertPerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.InsertRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.InsertEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertEventImpl implements InsertEvent {

    private final InsertPerformanceImpl insertPerformance;
    private final InsertRehearsalImpl insertRehearsal;

    @Override
    public void execute(final EventRequest eventRequest) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(eventRequest.getType())) {
            this.insertRehearsal.insertRehearsal(eventRequest);
        } else {
            this.insertPerformance.insertPerformance(eventRequest);
        }
    }

}
