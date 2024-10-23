package com.feyconsuelo.domain.usecase.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupRequest;

public interface UpdatePartitureGroup {

    void execute(Long partitureGroupId, PartitureGroupRequest partitureGroupRequest);

}
