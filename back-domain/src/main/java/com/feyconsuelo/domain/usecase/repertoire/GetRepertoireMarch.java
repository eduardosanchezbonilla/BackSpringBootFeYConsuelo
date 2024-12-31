package com.feyconsuelo.domain.usecase.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;

import java.util.Optional;

public interface GetRepertoireMarch {

    Optional<RepertoireMarchResponse> execute(Long repertoireMarchId);

}
