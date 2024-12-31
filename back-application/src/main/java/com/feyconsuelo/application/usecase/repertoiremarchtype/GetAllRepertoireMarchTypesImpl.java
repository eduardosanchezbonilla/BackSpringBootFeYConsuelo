package com.feyconsuelo.application.usecase.repertoiremarchtype;

import com.feyconsuelo.application.service.repertoiremarchtype.RepertoireMarchTypeService;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.GetAllRepertoireMarchTypes;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllRepertoireMarchTypesImpl implements GetAllRepertoireMarchTypes {

    private final RepertoireMarchTypeService repertoireMarchTypeService;

    @Override
    public List<RepertoireMarchTypeResponse> execute() {
        return this.repertoireMarchTypeService.getAll();
    }
}
