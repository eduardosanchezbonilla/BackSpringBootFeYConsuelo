package com.feyconsuelo.application.usecase.partituregroup;

import com.feyconsuelo.application.service.partituregroup.PartitureGroupService;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupRequest;
import com.feyconsuelo.domain.usecase.partituregroup.InsertPartitureGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class InsertPartitureGroupImpl implements InsertPartitureGroup {

    private final PartitureGroupService partitureGroupService;

    @Override
    public void execute(final PartitureGroupRequest partitureGroupRequest) {
        this.partitureGroupService.insert(partitureGroupRequest);
    }

}
