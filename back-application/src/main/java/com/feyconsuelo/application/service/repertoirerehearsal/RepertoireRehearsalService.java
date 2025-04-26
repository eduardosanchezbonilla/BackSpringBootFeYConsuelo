package com.feyconsuelo.application.service.repertoirerehearsal;

import com.feyconsuelo.domain.model.event.EventRepertoireResponse;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;

import java.util.List;
import java.util.Optional;

public interface RepertoireRehearsalService {

    void save(RepertoireEventRequest repertoireEventRequest);

    void logicalDelete(Long musicianId, Long eventId);

    List<RepertoireEventResponse> findAllActivesRepertoireMarchsByRehearsalId(Long rehearsalId, Boolean returnSolos);

    Optional<EventRepertoireResponse> getEventRepertoireRehearsal(Long eventId);
}