package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.usecase.user.DeleteUser;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DeleteUserImpl implements DeleteUser {

    private final UserService userService;

    @Override
    public void execute(final String username) {
        this.userService.logicalDelete(username);
    }
}
