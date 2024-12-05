package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.DeletePerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.DeleteRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.DeleteEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteEventImpl implements DeleteEvent {

    private final DeletePerformanceImpl deletePerformance;

    private final DeleteRehearsalImpl deleteRehearsal;

    @Override
    public void execute(final EventTypeEnum type, final Long eventId) {
        
        if (EventTypeEnum.PERFORMANCE.equals(type)) {
            this.deletePerformance.execute(eventId);
        } else {
            this.deleteRehearsal.execute(eventId);
        }

    }

}
