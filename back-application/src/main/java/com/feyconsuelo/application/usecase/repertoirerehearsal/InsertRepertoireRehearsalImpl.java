package com.feyconsuelo.application.usecase.repertoirerehearsal;

import com.feyconsuelo.application.service.repertoirerehearsal.RepertoireRehearsalService;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertRepertoireRehearsalImpl {

    private final RepertoireRehearsalService repertoireRehearsalService;

    public void insertRepertoireRehearsal(final RepertoireEventRequest repertoireEventRequest) {
        // insertamos
        this.repertoireRehearsalService.save(repertoireEventRequest);
    }

}
