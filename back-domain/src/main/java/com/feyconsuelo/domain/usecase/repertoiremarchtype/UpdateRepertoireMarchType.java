package com.feyconsuelo.domain.usecase.repertoiremarchtype;

import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeRequest;

public interface UpdateRepertoireMarchType {

    void execute(Long repertoireMarchTypeId, RepertoireMarchTypeRequest repertoireMarchTypeRequest);

}
