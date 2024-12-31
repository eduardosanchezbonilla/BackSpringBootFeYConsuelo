package com.feyconsuelo.domain.usecase.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;

import java.util.Optional;

public interface GetRepertoireCategory {

    Optional<RepertoireCategoryResponse> execute(Long repertoireCategoryId);

}
