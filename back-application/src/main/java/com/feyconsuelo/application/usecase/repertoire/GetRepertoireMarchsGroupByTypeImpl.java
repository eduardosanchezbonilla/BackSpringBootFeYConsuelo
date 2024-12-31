package com.feyconsuelo.application.usecase.repertoire;

import com.feyconsuelo.application.service.repertoire.RepertoireMarchService;
import com.feyconsuelo.application.service.repertoiremarchtype.RepertoireMarchTypeService;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeRequest;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchGroupByTypeResponse;
import com.feyconsuelo.domain.model.repertoire.RepertoireMarchResponse;
import com.feyconsuelo.domain.model.repertoiremarchtype.RepertoireMarchTypeResponse;
import com.feyconsuelo.domain.usecase.repertoire.GetRepertoireMarchsGroupByType;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetRepertoireMarchsGroupByTypeImpl implements GetRepertoireMarchsGroupByType {

    private final RepertoireMarchTypeService repertoireMarchTypeService;
    private final RepertoireMarchService repertoireMarchService;

    private Boolean filterRepertoireMarch(final RepertoireMarchResponse march, final RepertoireMarchGroupByTypeRequest repertoireMarchGroupByTypeRequest) {
        if (StringUtils.isEmpty(repertoireMarchGroupByTypeRequest.getName())) {
            return Boolean.TRUE;
        } else {
            return (march.getName()).toUpperCase().contains(repertoireMarchGroupByTypeRequest.getName().toUpperCase());
        }
    }

    private List<RepertoireMarchResponse> filterRepertoireMarchs(final List<RepertoireMarchResponse> repertoireMarchs,
                                                                 final RepertoireMarchGroupByTypeRequest repertoireMarchGroupByTypeRequest) {

        return Boolean.TRUE.equals(CollectionUtils.isEmpty(repertoireMarchs)) ?
                repertoireMarchs :
                repertoireMarchs.stream()
                        .filter(march -> this.filterRepertoireMarch(march, repertoireMarchGroupByTypeRequest))
                        .filter(march -> Boolean.FALSE.equals(repertoireMarchGroupByTypeRequest.getCurrent()) || Boolean.TRUE.equals(march.getCategory().getCurrent()))
                        .toList();
    }

    @Override
    public List<RepertoireMarchGroupByTypeResponse> execute(final RepertoireMarchGroupByTypeRequest repertoireMarchGroupByTypeRequest) {

        // obtenemos todos los
        final List<RepertoireMarchTypeResponse> types = this.repertoireMarchTypeService.getAll();

        // obtenemos todas las marchas por repertorio
        final List<RepertoireMarchResponse> repertoireMarchs = this.repertoireMarchService.getAll();

        // filtramos los marchas por nombre por voz
        final List<RepertoireMarchResponse> filterRepertoireMarchs = this.filterRepertoireMarchs(repertoireMarchs, repertoireMarchGroupByTypeRequest);

        // recorremos todas las voces y en cada una de ellos metemos los musicos que coincidan en voz
        return types.stream()
                .map(
                        type -> RepertoireMarchGroupByTypeResponse.builder()
                                .type(type)
                                .marchs(
                                        filterRepertoireMarchs.stream()
                                                .filter(march -> march.getType().getId().equals(type.getId()))
                                                .toList()
                                )
                                .build()
                )
                .filter(repertoireMarchGroupByTypeResponse -> Boolean.FALSE.equals(repertoireMarchGroupByTypeResponse.getMarchs().isEmpty()))
                .toList();
    }
}
