package com.feyconsuelo.application.service.repertoiremarchtype;


import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeRequest;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;

import java.util.List;
import java.util.Optional;

public interface RepertoireMarchTypeService {

    void delete(Long repertoireMarchTypeId);

    void logicalDelete(Long repertoireMarchTypeId);

    List<RepertoireMarchTypeResponse> getAll();

    Optional<RepertoireMarchTypeResponse> get(Long repertoireMarchTypeId);

    void insert(RepertoireMarchTypeRequest repertoireMarchTypeRequest);

    void update(Long repertoireMarchTypeId, RepertoireMarchTypeRequest repertoireMarchTypeRequest);

}
