package com.feyconsuelo.application.usecase.repertoiremarchtype;

import com.feyconsuelo.application.service.repertoiremarchtype.RepertoireMarchTypeService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeRequest;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.InsertRepertoireMarchType;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertRepertoireMarchTypeImpl implements InsertRepertoireMarchType {

    private final RepertoireMarchTypeService repertoireMarchTypeService;

    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.repertoire-march-type}")
    private String defaultRepertoireMarchTypeImage;

    @Override
    public void execute(final RepertoireMarchTypeRequest repertoireMarchTypeRequest) {

        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(repertoireMarchTypeRequest.getImage()) && !repertoireMarchTypeRequest.getImage().equals(this.defaultRepertoireMarchTypeImage)) {
            repertoireMarchTypeRequest.setImage(this.resizeImageService.resizeImage(repertoireMarchTypeRequest.getImage()));
        }

        this.repertoireMarchTypeService.insert(repertoireMarchTypeRequest);
    }

}
