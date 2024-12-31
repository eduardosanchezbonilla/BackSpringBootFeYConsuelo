package com.feyconsuelo.application.service.repertoireperformance;

import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;

import java.util.List;

public interface RepertoirePerformanceService {

    void save(RepertoireEventRequest repertoireEventRequest);

    void logicalDelete(Long marchId, Long eventId);

    List<RepertoireEventResponse> findAllActivesRepertoireMarchsByPerformanceId(Long performanceId);
}
