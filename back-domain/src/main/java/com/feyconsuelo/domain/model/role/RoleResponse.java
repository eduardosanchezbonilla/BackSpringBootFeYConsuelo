package com.feyconsuelo.domain.model.role;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RoleResponse {

    private String role;

    private String roleName;

}
