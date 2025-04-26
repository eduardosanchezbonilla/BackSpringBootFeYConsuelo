package com.feyconsuelo.application.service.repertoireperformance;

import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;

import java.util.List;
import java.util.Optional;

public interface RepertoirePerformanceService {

    void save(RepertoireEventRequest repertoireEventRequest);

    void logicalDelete(Long marchId, Long eventId);

    List<RepertoireEventResponse> findAllActivesRepertoireMarchsByPerformanceId(Long performanceId, final Boolean returnSolos);

    Optional<EventRepertoireResponse> getEventRepertoireRehearsal(Long eventId);
}
