package com.feyconsuelo.application.usecase.partituregroup;

import com.feyconsuelo.application.service.partituregroup.PartitureGroupService;
import com.feyconsuelo.application.usecase.image.ResizeImageImpl;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupRequest;
import com.feyconsuelo.domain.usecase.partituregroup.UpdatePartitureGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdatePartitureGroupImpl implements UpdatePartitureGroup {

    private final PartitureGroupService partitureGroupService;
    private final ResizeImageImpl resizeImageService;

    @Override
    public void execute(final Long partitureGroupId, final PartitureGroupRequest partitureGroupRequest) {
        this.partitureGroupService.update(partitureGroupId, partitureGroupRequest);
    }

}
