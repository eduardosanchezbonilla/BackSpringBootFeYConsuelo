package com.feyconsuelo.application.usecase.repertoiremarchtype;

import com.feyconsuelo.application.service.repertoiremarchtype.RepertoireMarchTypeService;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.domain.usecase.repertoiremarchtype.GetRepertoireMarchType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetRepertoireMarchTypeImpl implements GetRepertoireMarchType {

    private final RepertoireMarchTypeService repertoireMarchTypeService;

    @Override
    public Optional<RepertoireMarchTypeResponse> execute(final Long repertoireMarchTypeId) {
        return this.repertoireMarchTypeService.get(repertoireMarchTypeId);
    }
}
