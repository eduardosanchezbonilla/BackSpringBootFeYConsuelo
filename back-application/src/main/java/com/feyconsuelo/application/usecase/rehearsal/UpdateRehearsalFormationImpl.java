package com.feyconsuelo.application.usecase.rehearsal;

import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.exception.NotFoundException;
import com.feyconsuelo.domain.model.event.EventFormationRequest;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class UpdateRehearsalFormationImpl {

    private final RehearsalService rehearsalService;

    public void update(final Long eventId, final EventFormationRequest eventFormationRequest) {

        final Optional<EventResponse> eventResponse = this.rehearsalService.getById(eventId);

        if (eventResponse.isEmpty()) {
            throw new NotFoundException("No existe el ensayo que desea actualizar");
        }

        this.rehearsalService.updateFormation(eventResponse.get().getId(), eventFormationRequest);
    }

}
