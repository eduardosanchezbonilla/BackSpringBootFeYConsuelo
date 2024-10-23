package com.feyconsuelo.application.service.userpartituregroup;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupRequest;
import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;

import java.util.List;

public interface UserPartitureGroupService {

    void insert(UserPartitureGroupRequest userPartitureGroupRequest);

    void delete(UserPartitureGroupRequest userPartitureGroupRequest);

    void logicalDelete(UserPartitureGroupRequest userPartitureGroupRequest);

    List<UserPartitureGroupResponse> getAllUserPartitureGroups(String username);

    List<UserPartitureGroupResponse> getUserWithPartitureGroup(final Long partitureGroupId);
}
