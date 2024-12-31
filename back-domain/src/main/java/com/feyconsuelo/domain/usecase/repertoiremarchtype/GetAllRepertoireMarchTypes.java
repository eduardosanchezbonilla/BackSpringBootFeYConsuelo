package com.feyconsuelo.domain.usecase.repertoiremarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;

import java.util.List;

public interface GetAllRepertoireMarchTypes {

    List<RepertoireMarchTypeResponse> execute();

}
