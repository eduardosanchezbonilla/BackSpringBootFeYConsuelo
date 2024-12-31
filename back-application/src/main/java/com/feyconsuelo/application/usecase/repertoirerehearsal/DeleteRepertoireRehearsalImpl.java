package com.feyconsuelo.application.usecase.repertoirerehearsal;

import com.feyconsuelo.application.service.repertoirerehearsal.RepertoireRehearsalService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteRepertoireRehearsalImpl {

    private final RepertoireRehearsalService repertoireRehearsalService;

    public void execute(final Long marchId, final Long eventId) {
        this.repertoireRehearsalService.logicalDelete(marchId, eventId);
    }

}
