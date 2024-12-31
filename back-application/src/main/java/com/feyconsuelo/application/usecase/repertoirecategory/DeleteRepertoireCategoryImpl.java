package com.feyconsuelo.application.usecase.repertoirecategory;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.application.service.repertoirecategory.RepertoireCategoryService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.usecase.repertoirecategory.DeleteRepertoireCategory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class DeleteRepertoireCategoryImpl implements DeleteRepertoireCategory {

    private final RepertoireCategoryService repertoireCategoryService;
    private final RepertoireMarchService repertoireMarchService;
    
    @Override
    public void execute(final Long repertoireCategoryId) {
        // solo permitiremos borrar categorias que no esten asociadas a ningun repertoire
        final List<RepertoireMarchResponse> marchs = this.repertoireMarchService.getAllByCategoryId(repertoireCategoryId);
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(marchs))) {
            throw new BadRequestException("No se pueden eliminar categorias asociadas a marchas de repertorio");
        }
        this.repertoireCategoryService.logicalDelete(repertoireCategoryId);
    }

}
