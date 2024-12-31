package com.feyconsuelo.application.usecase.repertoireperformance;

import com.feyconsuelo.application.service.repertoireperformance.RepertoirePerformanceService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteRepertoirePerformanceImpl {

    private final RepertoirePerformanceService repertoirePerformanceService;

    public void execute(final Long marchId, final Long eventId) {
        this.repertoirePerformanceService.logicalDelete(marchId, eventId);
    }

}
