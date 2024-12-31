package com.feyconsuelo.application.usecase.repertoirecategory;

import com.feyconsuelo.application.service.repertoirecategory.RepertoireCategoryService;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.domain.usecase.repertoirecategory.GetAllRepertoireCategories;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllRepertoireCategoriesImpl implements GetAllRepertoireCategories {

    private final RepertoireCategoryService repertoireCategoryService;

    @Override
    public List<RepertoireCategoryResponse> execute() {
        return this.repertoireCategoryService.getAll();
    }
}
