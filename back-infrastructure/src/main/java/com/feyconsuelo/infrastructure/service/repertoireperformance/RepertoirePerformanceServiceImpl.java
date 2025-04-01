package com.feyconsuelo.infrastructure.service.repertoireperformance;

import com.feyconsuelo.application.service.repertoireperformance.RepertoirePerformanceService;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventResponse;
import com.feyconsuelo.infrastructure.converter.repertoireperformance.RepertoireEventRequestToRepertoirePerformanceEntityConverter;
import com.feyconsuelo.infrastructure.converter.repertoireperformance.RepertoirePerformanceEntityListToRepertoireEventResponseListConverter;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformanceEntity;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformancePK;
import com.feyconsuelo.infrastructure.repository.RepertoirePerformanceRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class RepertoirePerformanceServiceImpl implements RepertoirePerformanceService {

    private final RepertoirePerformanceRepository repertoirePerformanceRepository;
    private final RepertoireEventRequestToRepertoirePerformanceEntityConverter repertoireEventRequestToRepertoirePerformanceEntityConverter;
    private final RepertoirePerformanceEntityListToRepertoireEventResponseListConverter repertoirePerformanceEntityListToRepertoireEventResponseListConverter;

    @Override
    public void save(final RepertoireEventRequest repertoireEventRequest) {
        this.repertoirePerformanceRepository.save(
                this.repertoireEventRequestToRepertoirePerformanceEntityConverter.convert(repertoireEventRequest)
        );
    }

    @Override
    public void logicalDelete(final Long marchId, final Long eventId) {
        final Optional<RepertoirePerformanceEntity> event = this.repertoirePerformanceRepository.findById(
                RepertoirePerformancePK.builder()
                        .marchId(marchId)
                        .performanceId(eventId)
                        .build()
        );

        event.ifPresent(repertoirePerformanceEntity -> this.repertoirePerformanceRepository.save(this.repertoireEventRequestToRepertoirePerformanceEntityConverter.deleteEntity(repertoirePerformanceEntity)));
    }

    @Override
    public List<RepertoireEventResponse> findAllActivesRepertoireMarchsByPerformanceId(final Long performanceId, final Boolean returnSolos) {
        final List<RepertoirePerformanceEntity> marchList = this.repertoirePerformanceRepository.findAllActivesRepertoireMarchsByPerformanceId(performanceId);
        return this.repertoirePerformanceEntityListToRepertoireEventResponseListConverter.convert(marchList, returnSolos);
    }

}
