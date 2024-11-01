package com.feyconsuelo.domain.usecase.user;

import com.feyconsuelo.domain.model.user.UserGroupByRoleRequest;
import com.feyconsuelo.domain.model.user.UserGroupByRoleResponse;

import java.util.List;

public interface GetUsersGroupByRole {

    List<UserGroupByRoleResponse> execute(final UserGroupByRoleRequest userGroupByRoleRequest);

}
