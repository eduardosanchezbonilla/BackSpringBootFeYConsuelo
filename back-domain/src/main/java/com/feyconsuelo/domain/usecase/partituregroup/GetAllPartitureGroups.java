package com.feyconsuelo.domain.usecase.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;

import java.util.List;

public interface GetAllPartitureGroups {

    List<PartitureGroupResponse> execute();

}
