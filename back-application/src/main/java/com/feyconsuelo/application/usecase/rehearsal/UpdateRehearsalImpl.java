package com.feyconsuelo.application.usecase.rehearsal;

import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateRehearsalImpl {

    private final RehearsalService rehearsalService;

    public void update(final Long eventId, final EventRequest eventRequest) {

        final Optional<EventResponse> eventResponse = this.rehearsalService.getById(eventId);

        if (eventResponse.isEmpty()) {
            throw new NotFoundException("No existe el ensayo que desea actualizar");
        }

        this.rehearsalService.update(eventResponse.get().getId(), eventRequest);

    }

}
