package com.feyconsuelo.application.usecase.rehearsal;

import com.feyconsuelo.application.service.rehearsal.RehearsalService;
import com.feyconsuelo.domain.model.event.EventResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetRehearsalImpl {

    private final RehearsalService rehearsalService;

    public Optional<EventResponse> execute(final Long eventId) {
        return this.rehearsalService.getById(eventId);
    }
}
