package com.feyconsuelo.infrastructure.service.repertoirerehearsal;

import com.feyconsuelo.application.service.repertoirerehearsal.RepertoireRehearsalService;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;
import com.feyconsuelo.infrastructure.converter.repertoirerehearsal.RepertoireEventRequestToRepertoireRehearsalEntityConverter;
import com.feyconsuelo.infrastructure.converter.repertoirerehearsal.RepertoireRehearsalEntityListToRepertoireEventResponseListConverter;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalPK;
import com.feyconsuelo.infrastructure.repository.RepertoireRehearsalRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoireRehearsalServiceImpl implements RepertoireRehearsalService {

    private final RepertoireRehearsalRepository repertoireRehearsalRepository;
    private final RepertoireEventRequestToRepertoireRehearsalEntityConverter repertoireEventRequestToRepertoireRehearsalEntityConverter;
    private final RepertoireRehearsalEntityListToRepertoireEventResponseListConverter repertoireRehearsalEntityListToRepertoireEventResponseListConverter;

    @Override
    public void save(final RepertoireEventRequest repertoireEventRequest) {
        this.repertoireRehearsalRepository.save(
                this.repertoireEventRequestToRepertoireRehearsalEntityConverter.convert(repertoireEventRequest)
        );
    }

    @Override
    public void logicalDelete(final Long marchId, final Long eventId) {
        final Optional<RepertoireRehearsalEntity> event = this.repertoireRehearsalRepository.findById(
                RepertoireRehearsalPK.builder()
                        .marchId(marchId)
                        .rehearsalId(eventId)
                        .build()
        );

        event.ifPresent(repertoireRehearsalEntity -> this.repertoireRehearsalRepository.save(this.repertoireEventRequestToRepertoireRehearsalEntityConverter.deleteEntity(repertoireRehearsalEntity)));
    }

    @Override
    public List<RepertoireEventResponse> findAllActivesRepertoireMarchsByRehearsalId(final Long rehearsalId, final Boolean returnSolos) {
        final List<RepertoireRehearsalEntity> marchList = this.repertoireRehearsalRepository.findAllActivesRepertoireMarchsByRehearsalId(rehearsalId);
        return this.repertoireRehearsalEntityListToRepertoireEventResponseListConverter.convert(marchList, returnSolos);
    }

}
