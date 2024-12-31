package com.feyconsuelo.application.usecase.repertoireevent;

import com.feyconsuelo.application.usecase.repertoireperformance.DeleteRepertoirePerformanceImpl;
import com.feyconsuelo.application.usecase.repertoirerehearsal.DeleteRepertoireRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.repertoireevent.DeleteRepertoireEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteRepertoireEventImpl implements DeleteRepertoireEvent {

    private final DeleteRepertoirePerformanceImpl deletePerformance;

    private final DeleteRepertoireRehearsalImpl deleteRehearsal;

    @Override
    public void execute(final Long marchId, final EventTypeEnum type, final Long eventId) {

        if (EventTypeEnum.PERFORMANCE.equals(type)) {
            this.deletePerformance.execute(marchId, eventId);
        } else {
            this.deleteRehearsal.execute(marchId, eventId);
        }

    }

}
