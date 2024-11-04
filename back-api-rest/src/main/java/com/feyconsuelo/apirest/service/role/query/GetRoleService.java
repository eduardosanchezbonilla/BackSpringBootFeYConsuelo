package com.feyconsuelo.apirest.service.role.query;

import com.feyconsuelo.apirest.converter.role.RoleResponseListToRoleResponseDtoListConverter;
import com.feyconsuelo.domain.model.role.RoleResponse;
import com.feyconsuelo.domain.usecase.role.GetAllRoles;
import com.feyconsuelo.openapi.model.RoleResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class GetRoleService {

    private final GetAllRoles getAllRoles;

    private final RoleResponseListToRoleResponseDtoListConverter roleResponseListToRoleResponseDtoListConverter;

    public ResponseEntity<List<RoleResponseDto>> getAllRoles() {
        final List<RoleResponse> roleResponseList = this.getAllRoles.execute();
        if (CollectionUtils.isEmpty(roleResponseList)) {
            return ResponseEntity.noContent().build();
        }
        return ResponseEntity.ok(this.roleResponseListToRoleResponseDtoListConverter.convert(roleResponseList));
    }

}
