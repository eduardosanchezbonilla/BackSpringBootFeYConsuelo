package com.feyconsuelo.apirest.service.user.update;

import com.feyconsuelo.apirest.validate.user.ValidateUserService;
import com.feyconsuelo.domain.model.user.UpdateUserRolesRequest;
import com.feyconsuelo.domain.usecase.user.UpdateUserRoles;
import com.feyconsuelo.openapi.model.UpdateUserRolesDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class UpdateUserRolesService {

    private final UpdateUserRoles updateUserRoles;

    private final ValidateUserService validateUserService;

    public ResponseEntity<Void> updateUserRoles(final String username,
                                                final UpdateUserRolesDto updateUserRolesDto) {
        this.validateUserService.validate(updateUserRolesDto);
        this.updateUserRoles.execute(
                UpdateUserRolesRequest.builder()
                        .username(username.toLowerCase())
                        .roles(updateUserRolesDto.getRoles())
                        .build()
        );
        return ResponseEntity.status(HttpStatus.OK).build();
    }
}
