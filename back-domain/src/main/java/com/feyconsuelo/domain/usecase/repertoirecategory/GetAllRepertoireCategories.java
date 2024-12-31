package com.feyconsuelo.domain.usecase.repertoirecategory;

import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;

import java.util.List;

public interface GetAllRepertoireCategories {

    List<RepertoireCategoryResponse> execute();

}
