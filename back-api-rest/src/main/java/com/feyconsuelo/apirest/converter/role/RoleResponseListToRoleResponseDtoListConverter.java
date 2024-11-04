package com.feyconsuelo.apirest.converter.role;

import com.feyconsuelo.domain.model.role.RoleResponse;
import com.feyconsuelo.openapi.model.RoleResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RoleResponseListToRoleResponseDtoListConverter {

    private final RoleResponseToRoleResponseDtoConverter roleResponseToRoleResponseDtoConverter;

    public List<RoleResponseDto> convert(final List<RoleResponse> roleResponseList) {
        if (CollectionUtils.isEmpty(roleResponseList)) {
            return List.of();
        }
        return roleResponseList.stream().map(this.roleResponseToRoleResponseDtoConverter::convert).toList();
    }

}
