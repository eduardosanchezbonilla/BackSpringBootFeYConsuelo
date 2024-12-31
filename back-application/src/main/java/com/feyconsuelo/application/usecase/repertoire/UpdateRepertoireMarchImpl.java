package com.feyconsuelo.application.usecase.repertoire;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchRequest;
import com.feyconsuelo.domain.usecase.repertoire.UpdateRepertoireMarch;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateRepertoireMarchImpl implements UpdateRepertoireMarch {

    private final RepertoireMarchService repertoireMarchService;
    private final ResizeImageImpl resizeImageService;

    @Value("${default-images.repertoire-march}")
    private String defaultRepertoireMarchImage;

    @Override
    public void execute(final Long repertoireMarchId, final RepertoireMarchRequest repertoireMarchRequest) {
        // si estan enviando imagen, debemos redimensionarla
        if (StringUtils.isNotEmpty(repertoireMarchRequest.getImage()) && !repertoireMarchRequest.getImage().equals(this.defaultRepertoireMarchImage)) {
            repertoireMarchRequest.setImage(this.resizeImageService.resizeImage(repertoireMarchRequest.getImage()));
        }

        this.repertoireMarchService.update(repertoireMarchId, repertoireMarchRequest);
    }

}
