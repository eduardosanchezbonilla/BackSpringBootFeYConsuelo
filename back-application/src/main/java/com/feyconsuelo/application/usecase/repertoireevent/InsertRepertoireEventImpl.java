package com.feyconsuelo.application.usecase.repertoireevent;

import com.feyconsuelo.application.usecase.repertoireperformance.InsertRepertoirePerformanceImpl;
import com.feyconsuelo.application.usecase.repertoirerehearsal.InsertRepertoireRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.domain.usecase.repertoireevent.InsertRepertoireEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertRepertoireEventImpl implements InsertRepertoireEvent {

    private final InsertRepertoirePerformanceImpl insertPerformance;
    private final InsertRepertoireRehearsalImpl insertRehearsal;

    @Override
    public void execute(final RepertoireEventRequest repertoireEventRequest) {
        // dependiendo del tipo de evento insertaremos en Actuaciones o Ensayos
        if (EventTypeEnum.REHEARSAL.equals(repertoireEventRequest.getEventType())) {
            this.insertRehearsal.insertRepertoireRehearsal(repertoireEventRequest);
        } else {
            this.insertPerformance.insertRepertoirePerformance(repertoireEventRequest);
        }
    }

}
