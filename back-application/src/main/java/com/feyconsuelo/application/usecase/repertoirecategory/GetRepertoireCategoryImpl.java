package com.feyconsuelo.application.usecase.repertoirecategory;

import com.feyconsuelo.application.service.repertoirecategory.RepertoireCategoryService;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryResponse;
import com.feyconsuelo.domain.usecase.repertoirecategory.GetRepertoireCategory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetRepertoireCategoryImpl implements GetRepertoireCategory {

    private final RepertoireCategoryService repertoireCategoryService;

    @Override
    public Optional<RepertoireCategoryResponse> execute(final Long repertoireCategoryId) {
        return this.repertoireCategoryService.get(repertoireCategoryId);
    }
}
