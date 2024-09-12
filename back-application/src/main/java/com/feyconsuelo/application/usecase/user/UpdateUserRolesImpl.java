package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.model.user.UpdateUserRolesRequest;
import com.feyconsuelo.domain.usecase.user.UpdateUserRoles;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class UpdateUserRolesImpl implements UpdateUserRoles {

    private final UserService userService;

    @Override
    public void execute(final UpdateUserRolesRequest updateUserRolesRequest) {
        this.userService.updateRoles(updateUserRolesRequest.getUsername(), updateUserRolesRequest.getRoles());
    }
}
