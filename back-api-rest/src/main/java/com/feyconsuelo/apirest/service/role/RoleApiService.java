package com.feyconsuelo.apirest.service.role;

import com.feyconsuelo.apirest.service.role.query.GetRoleService;
import com.feyconsuelo.openapi.api.RoleControllerApiDelegate;
import com.feyconsuelo.openapi.model.RoleResponseDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class RoleApiService implements RoleControllerApiDelegate {

    private final GetRoleService getRoleService;

    @Override
    public ResponseEntity<List<RoleResponseDto>> getRoles() {
        return this.getRoleService.getAllRoles();
    }

}
