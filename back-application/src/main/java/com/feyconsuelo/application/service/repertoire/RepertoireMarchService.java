package com.feyconsuelo.application.service.repertoire;


import com.feyconsuelo.domain.model.repertoire.RepertoireMarchRequest;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;

import java.util.List;
import java.util.Optional;

public interface RepertoireMarchService {

    void delete(Long repertoireMarchId);

    void logicalDelete(Long repertoireMarchId);

    List<RepertoireMarchResponse> getAll(final Boolean returnSolos);

    Optional<RepertoireMarchResponse> get(Long repertoireMarchId);

    void insert(RepertoireMarchRequest repertoireMarchRequest);

    void update(Long repertoireMarchId, RepertoireMarchRequest repertoireMarchRequest);

    List<RepertoireMarchResponse> getAllByCategoryId(Long categoryId);

    List<RepertoireMarchResponse> getAllByTypeId(Long typeId);
}
