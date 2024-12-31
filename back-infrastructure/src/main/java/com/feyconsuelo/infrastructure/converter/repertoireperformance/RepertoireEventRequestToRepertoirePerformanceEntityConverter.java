package com.feyconsuelo.infrastructure.converter.repertoireperformance;

import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.infrastructure.entities.performance.PerformanceEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformanceEntity;
import com.feyconsuelo.infrastructure.entities.repertoireperformance.RepertoirePerformancePK;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireEventRequestToRepertoirePerformanceEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public RepertoirePerformanceEntity convert(final RepertoireEventRequest repertoireEventRequest) {
        return RepertoirePerformanceEntity.builder()
                .id(
                        RepertoirePerformancePK.builder()
                                .marchId(repertoireEventRequest.getMarchId())
                                .performanceId(repertoireEventRequest.getEventId())
                                .build()
                )
                .march(
                        RepertoireMarchEntity.builder()
                                .id(repertoireEventRequest.getMarchId())
                                .build()
                )
                .performance(
                        PerformanceEntity.builder()
                                .id(repertoireEventRequest.getEventId())
                                .build()
                )
                .updateUserRP(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public RepertoirePerformanceEntity deleteEntity(final RepertoirePerformanceEntity repertoirePerformanceEntity) {
        repertoirePerformanceEntity.setDeleteDateRP(LocalDateTime.now());
        repertoirePerformanceEntity.setUpdateUserRP(this.tokenInfoExtractorService.getUsername());
        return repertoirePerformanceEntity;
    }
}
