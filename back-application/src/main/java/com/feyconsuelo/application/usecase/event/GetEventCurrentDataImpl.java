package com.feyconsuelo.application.usecase.event;

import com.feyconsuelo.application.usecase.performance.GetPerformanceCurrentDataImpl;
import com.feyconsuelo.domain.exception.FeYConsueloException;
import com.feyconsuelo.domain.model.event.EventCurrentDataResponse;
import com.feyconsuelo.domain.model.event.EventTypeEnum;
import com.feyconsuelo.domain.usecase.event.GetEventCurrentData;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetEventCurrentDataImpl implements GetEventCurrentData {

    private final GetPerformanceCurrentDataImpl getPerformanceCurrentData;

    @Override
    public Optional<EventCurrentDataResponse> execute(final EventTypeEnum eventType, final Long eventId) {
        if (EventTypeEnum.PERFORMANCE.equals(eventType)) {
            return this.getPerformanceCurrentData.execute(eventId);
        } else {
            throw new FeYConsueloException("No existen datos online para ensayos");
        }
    }
}
