package com.feyconsuelo.domain.usecase.repertoiremarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;

import java.util.Optional;

public interface GetRepertoireMarchType {

    Optional<RepertoireMarchTypeResponse> execute(Long repertoireMarchTypeId);

}
