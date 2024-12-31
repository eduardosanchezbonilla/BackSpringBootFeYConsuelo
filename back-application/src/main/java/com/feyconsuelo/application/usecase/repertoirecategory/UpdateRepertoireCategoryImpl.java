package com.feyconsuelo.application.usecase.repertoirecategory;

import com.feyconsuelo.application.service.repertoirecategory.RepertoireCategoryService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.repertoirecategory.RepertoireCategoryRequest;
import com.feyconsuelo.domain.usecase.repertoirecategory.UpdateRepertoireCategory;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateRepertoireCategoryImpl implements UpdateRepertoireCategory {

    private final RepertoireCategoryService repertoireCategoryService;
    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.repertoire-category}")
    private String defaultRepertoireCategoryImage;

    @Override
    public void execute(final Long repertoireCategoryId, final RepertoireCategoryRequest repertoireCategoryRequest) {
        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(repertoireCategoryRequest.getImage()) && !repertoireCategoryRequest.getImage().equals(this.defaultRepertoireCategoryImage)) {
            repertoireCategoryRequest.setImage(this.resizeImageService.resizeImage(repertoireCategoryRequest.getImage(), 400, 0.8f));
        }

        this.repertoireCategoryService.update(repertoireCategoryId, repertoireCategoryRequest);
    }

}
