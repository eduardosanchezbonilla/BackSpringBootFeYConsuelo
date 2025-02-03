package com.feyconsuelo.infrastructure.converter.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchSoloist;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchMainSoloistEntity;
import com.feyconsuelo.infrastructure.entities.repertoire.RepertoireMarchSoloEntity;
import com.feyconsuelo.infrastructure.service.security.user.TokenInfoExtractorServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RepertoireMarchSoloistToRepertoireMarchMainSoloistEntityConverter {

    private final TokenInfoExtractorServiceImpl tokenInfoExtractorService;

    public RepertoireMarchMainSoloistEntity convert(final RepertoireMarchSoloEntity solo, final RepertoireMarchSoloist repertoireMarchSoloist) {
        return RepertoireMarchMainSoloistEntity.builder()
                .soloId(solo.getId().intValue())
                .solo(solo)
                .musicianId(repertoireMarchSoloist.getMusicianId())
                .musicianName(repertoireMarchSoloist.getMusicianName())
                .order(repertoireMarchSoloist.getOrder())
                .modifiedUserMarchMainSoloist(this.tokenInfoExtractorService.getUsername())
                .build();
    }

}
