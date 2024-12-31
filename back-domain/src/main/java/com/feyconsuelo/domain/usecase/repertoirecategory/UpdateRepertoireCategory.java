package com.feyconsuelo.domain.usecase.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryRequest;

public interface UpdateRepertoireCategory {

    void execute(Long repertoireCategoryId, RepertoireCategoryRequest repertoireCategoryRequest);

}
