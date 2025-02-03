package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.UpdatePerformanceFormationImpl;
import com.feyconsuelo.application.usecase.rehearsal.UpdateRehearsalFormationImpl;
import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.UpdateEventFormation;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateEventFormationImpl implements UpdateEventFormation {

    private final UpdatePerformanceFormationImpl updatePerformanceFormation;
    private final UpdateRehearsalFormationImpl updateRehearsalFormation;

    @Override
    public void execute(final EventTypeEnum type, final Long eventId, final EventFormationRequest eventFormationRequest) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(type)) {
            this.updateRehearsalFormation.update(eventId, eventFormationRequest);
        } else {
            this.updatePerformanceFormation.update(eventId, eventFormationRequest);
        }
    }

}
