package com.feyconsuelo.infrastructure.converter.repertoirerehearsal;

import com.feyconsuelo.domain.model.repertoireevent.RepertoireEventRequest;
import com.feyconsuelo.infrastructure.entities.rehearsal.RehearsalEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalEntity;
import com.feyconsuelo.infrastructure.entities.repertoirerehearsal.RepertoireRehearsalPK;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireEventRequestToRepertoireRehearsalEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public RepertoireRehearsalEntity convert(final RepertoireEventRequest repertoireEventRequest) {
        return RepertoireRehearsalEntity.builder()
                .id(
                        RepertoireRehearsalPK.builder()
                                .marchId(repertoireEventRequest.getMarchId())
                                .rehearsalId(repertoireEventRequest.getEventId())
                                .build()
                )
                .march(
                        RepertoireMarchEntity.builder()
                                .id(repertoireEventRequest.getMarchId())
                                .build()
                )
                .rehearsal(
                        RehearsalEntity.builder()
                                .id(repertoireEventRequest.getEventId())
                                .build()
                )
                .updateUserRR(this.tokenInfoExtractorService.getUsername())
                .build();
    }

    public RepertoireRehearsalEntity deleteEntity(final RepertoireRehearsalEntity repertoireRehearsalEntity) {
        repertoireRehearsalEntity.setDeleteDateRR(LocalDateTime.now());
        repertoireRehearsalEntity.setUpdateUserRR(this.tokenInfoExtractorService.getUsername());
        return repertoireRehearsalEntity;
    }
}
