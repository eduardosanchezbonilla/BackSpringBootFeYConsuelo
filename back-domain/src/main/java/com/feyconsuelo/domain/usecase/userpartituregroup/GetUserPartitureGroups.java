package com.feyconsuelo.domain.usecase.userpartituregroup;

import com.feyconsuelo.domain.model.userpatituregroup.UserPartitureGroupResponse;

import java.util.List;

public interface GetUserPartitureGroups {

    List<UserPartitureGroupResponse> execute(String username);

}
