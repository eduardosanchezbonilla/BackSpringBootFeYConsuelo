package com.feyconsuelo.domain.usecase.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeRequest;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;

import java.util.List;

public interface GetRepertoireMarchsGroupByType {

    List<RepertoireMarchGroupByTypeResponse> execute(final RepertoireMarchGroupByTypeRequest repertoireMarchGroupByTypeRequest);

}
