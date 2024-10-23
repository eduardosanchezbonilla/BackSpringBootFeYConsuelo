package com.feyconsuelo.application.service.partituregroup;

import com.feyconsuelo.domain.model.partituregroup.PartitureGroupRequest;
import com.feyconsuelo.domain.model.partituregroup.PartitureGroupResponse;

import java.util.List;
import java.util.Optional;

public interface PartitureGroupService {

    void delete(Long partitureGroupId);

    void logicalDelete(Long partitureGroupId);

    List<PartitureGroupResponse> getAll(final List<Long> partitureGroupIdList,
                                        final Boolean allPartitureGroups);

    Optional<PartitureGroupResponse> get(Long partitureGroupId);

    void insert(PartitureGroupRequest partitureGroupRequest);

    void update(Long partitureGroupId, PartitureGroupRequest partitureGroupRequest);

}
