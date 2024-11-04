package com.feyconsuelo.apirest.converter.role;

import com.feyconsuelo.domain.model.role.RoleResponse;
import com.feyconsuelo.openapi.model.RoleResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class RoleResponseToRoleResponseDtoConverter {

    public RoleResponseDto convert(final RoleResponse roleResponse) {
        return RoleResponseDto.builder()
                .role(roleResponse.getRole())
                .roleName(roleResponse.getRoleName())
                .build();
    }

}
