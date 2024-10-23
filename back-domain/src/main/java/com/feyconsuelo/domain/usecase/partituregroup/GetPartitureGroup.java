package com.feyconsuelo.domain.usecase.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;

import java.util.Optional;

public interface GetPartitureGroup {

    Optional<PartitureGroupResponse> execute(Long partitureGroupId);

}
