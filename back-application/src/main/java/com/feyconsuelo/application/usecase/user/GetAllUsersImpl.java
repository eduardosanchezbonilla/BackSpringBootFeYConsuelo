package com.feyconsuelo.application.usecase.user;

import com.feyconsuelo.application.service.user.UserService;
import com.feyconsuelo.domain.model.user.UserResponse;
import com.feyconsuelo.domain.usecase.user.GetAllUsers;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
public class GetAllUsersImpl implements GetAllUsers {

    private final UserService userService;

    @Override
    public List<UserResponse> execute() {
        return this.userService.getAll();
    }
}
