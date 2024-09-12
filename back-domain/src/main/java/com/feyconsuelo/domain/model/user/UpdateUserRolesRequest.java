package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Value;

import java.util.List;

@Value
@Builder
public class UpdateUserRolesRequest {

    String username;

    List<String> roles;

}
