package com.feyconsuelo.domain.usecase.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchRequest;

public interface UpdateRepertoireMarch {

    void execute(Long repertoireMarchId, RepertoireMarchRequest repertoireMarchRequest);

}
