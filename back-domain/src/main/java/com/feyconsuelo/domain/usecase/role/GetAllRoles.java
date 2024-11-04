package com.feyconsuelo.domain.usecase.role;

import com.feyconsuelo.domain.model.role.RoleResponse;

import java.util.List;

public interface GetAllRoles {

    List<RoleResponse> execute();

}
