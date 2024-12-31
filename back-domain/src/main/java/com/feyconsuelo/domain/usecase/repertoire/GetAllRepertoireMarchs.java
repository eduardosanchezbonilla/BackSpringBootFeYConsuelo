package com.feyconsuelo.domain.usecase.repertoire;

import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;

import java.util.List;

public interface GetAllRepertoireMarchs {

    List<RepertoireMarchResponse> execute();

}
