package com.feyconsuelo.application.usecase.repertoire;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.domain.usecase.repertoire.DeleteRepertoireMarch;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteRepertoireMarchImpl implements DeleteRepertoireMarch {

    private final RepertoireMarchService repertoireMarchService;
    
    @Override
    public void execute(final Long repertoireMarchId) {
        this.repertoireMarchService.logicalDelete(repertoireMarchId);
    }

}
