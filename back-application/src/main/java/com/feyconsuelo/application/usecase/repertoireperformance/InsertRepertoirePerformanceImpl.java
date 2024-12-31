package com.feyconsuelo.application.usecase.repertoireperformance;

import com.feyconsuelo.application.service.repertoireperformance.RepertoirePerformanceService;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertRepertoirePerformanceImpl {

    private final RepertoirePerformanceService repertoirePerformanceService;

    public void insertRepertoirePerformance(final RepertoireEventRequest repertoireEventRequest) {
        // insertamos
        this.repertoirePerformanceService.save(repertoireEventRequest);
    }

}
