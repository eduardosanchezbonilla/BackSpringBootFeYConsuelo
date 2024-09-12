package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.GetUser;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
public class GetUserImpl implements GetUser {

    private final UserService userService;

    @Override
    public Optional<UserResponse> execute(final String username) {
        return this.userService.get(username);
    }
}
