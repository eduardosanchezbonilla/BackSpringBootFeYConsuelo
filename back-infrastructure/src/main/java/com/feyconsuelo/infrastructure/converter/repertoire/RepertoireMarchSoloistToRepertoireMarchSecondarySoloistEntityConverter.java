package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSoloist;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchSecondarySoloistEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchSoloEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloistToRepertoireMarchSecondarySoloistEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public RepertoireMarchSecondarySoloistEntity convert(final RepertoireMarchSoloEntity solo, final RepertoireMarchSoloist repertoireMarchSoloist) {
        return RepertoireMarchSecondarySoloistEntity.builder()
                .soloId(solo.getId().intValue())
                .solo(solo)
                .musicianId(repertoireMarchSoloist.getMusicianId())
                .musicianName(repertoireMarchSoloist.getMusicianName())
                .order(repertoireMarchSoloist.getOrder())
                .modifiedUserSecondaryMarchMainSoloist(this.tokenInfoExtractorService.getUsername())
                .build();
    }

}
