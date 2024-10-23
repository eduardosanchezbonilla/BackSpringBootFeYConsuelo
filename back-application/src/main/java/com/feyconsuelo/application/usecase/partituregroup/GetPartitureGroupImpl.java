package com.feyconsuelo.application.usecase.partituregroup;

import com.feyconsuelo.application.service.partituregroup.PartitureGroupService;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;
import com.feyconsuelo.domain.usecase.partituregroup.GetPartitureGroup;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetPartitureGroupImpl implements GetPartitureGroup {

    private final PartitureGroupService partitureGroupService;

    @Override
    public Optional<PartitureGroupResponse> execute(final Long partitureGroupId) {
        return this.partitureGroupService.get(partitureGroupId);
    }
}
