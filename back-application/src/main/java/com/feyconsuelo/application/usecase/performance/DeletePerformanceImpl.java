package com.feyconsuelo.application.usecase.performance;

import com.feyconsuelo.application.service.performance.PerformanceService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class DeletePerformanceImpl {

    private final PerformanceService performanceService;

    public void execute(final Long eventId) {
        final Optional<EventResponse> eventResponse = this.performanceService.getById(eventId);

        if (eventResponse.isEmpty()) {
            throw new NotFoundException("No existe la actuacion que desea eliminar");
        } else {
            this.performanceService.logicalDelete(eventId);
        }
    }

}