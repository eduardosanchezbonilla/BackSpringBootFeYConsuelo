package com.feyconsuelo.application.usecase.repertoiremarchtype;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.application.service.repertoiremarchtype.RepertoireMarchTypeService;
import com.feyconsuelo.domain.exception.BadRequestException;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.DeleteRepertoireMarchType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class DeleteRepertoireMarchTypeImpl implements DeleteRepertoireMarchType {

    private final RepertoireMarchTypeService repertoireMarchTypeService;
    private final RepertoireMarchService repertoireMarchService;

    @Override
    public void execute(final Long repertoireMarchTypeId) {
        // solo permitiremos borrar tipos que no esten asociadas a ningun repertoire
        final List<RepertoireMarchResponse> marchs = this.repertoireMarchService.getAllByTypeId(repertoireMarchTypeId);
        if (Boolean.FALSE.equals(CollectionUtils.isEmpty(marchs))) {
            throw new BadRequestException("No se pueden eliminar tipos asociadas a marchas de repertorio");
        }
        this.repertoireMarchTypeService.logicalDelete(repertoireMarchTypeId);
    }

}
