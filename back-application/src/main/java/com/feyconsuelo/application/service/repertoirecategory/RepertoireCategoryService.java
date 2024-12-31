package com.feyconsuelo.application.service.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryRequest;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;

import java.util.List;
import java.util.Optional;

public interface RepertoireCategoryService {

    void delete(Long repertoireCategoryId);

    void logicalDelete(Long repertoireCategoryId);

    List<RepertoireCategoryResponse> getAll();

    Optional<RepertoireCategoryResponse> get(Long repertoireCategoryId);

    void insert(RepertoireCategoryRequest repertoireCategoryRequest);

    void update(Long repertoireCategoryId, RepertoireCategoryRequest repertoireCategoryRequest);

}
