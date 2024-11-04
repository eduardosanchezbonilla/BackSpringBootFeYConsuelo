package com.feyconsuelo.application.usecase.role;

import com.feyconsuelo.domain.model.role.RoleResponse;
import com.feyconsuelo.domain.model.user.UserRoleEnum;
import com.feyconsuelo.domain.usecase.role.GetAllRoles;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Stream;

@Component
@RequiredArgsConstructor
public class GetAllRolesImpl implements GetAllRoles {

    @Override
    public List<RoleResponse> execute() {
        return Stream.of(UserRoleEnum.values())
                .map(role -> RoleResponse.builder()
                        .role(role.name())
                        .roleName(role.getRoleName())
                        .build()
                )
                .toList();
    }
}
