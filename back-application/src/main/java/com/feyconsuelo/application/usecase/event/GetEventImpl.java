package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.GetPerformanceImpl;
import com.feyconsuelo.application.usecase.rehearsal.GetRehearsalImpl;
import com.feyconsuelo.domain.model.event.EventResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.GetEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventImpl implements GetEvent {

    private final GetPerformanceImpl getPerformance;
    private final GetRehearsalImpl getRehearsal;

    @Override
    public Optional<EventResponse> execute(final EventTypeEnum eventType, final Long eventId) {
        if (EventTypeEnum.PERFORMANCE.equals(eventType)) {
            return this.getPerformance.execute(eventId);
        } else {
            return this.getRehearsal.execute(eventId);
        }
    }
}
