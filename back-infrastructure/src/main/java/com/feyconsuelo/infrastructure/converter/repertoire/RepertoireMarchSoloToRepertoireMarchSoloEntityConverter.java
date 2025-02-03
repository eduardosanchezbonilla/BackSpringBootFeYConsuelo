package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSolo;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchSoloEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloToRepertoireMarchSoloEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;
    
    public RepertoireMarchSoloEntity convert(final RepertoireMarchEntity march, final RepertoireMarchSolo repertoireMarchSolo) {
        return RepertoireMarchSoloEntity.builder()
                .marchId(march.getId().intValue())
                .march(march)
                .name(repertoireMarchSolo.getName())
                .order(repertoireMarchSolo.getOrder())
                .modifiedUserMarchSolo(this.tokenInfoExtractorService.getUsername())
                .build();
    }

}
