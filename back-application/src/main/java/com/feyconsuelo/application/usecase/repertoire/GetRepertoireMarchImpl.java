package com.feyconsuelo.application.usecase.repertoire;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.usecase.repertoire.GetRepertoireMarch;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetRepertoireMarchImpl implements GetRepertoireMarch {

    private final RepertoireMarchService repertoireMarchService;

    @Override
    public Optional<RepertoireMarchResponse> execute(final Long repertoireMarchId) {
        return this.repertoireMarchService.get(repertoireMarchId);
    }
}
