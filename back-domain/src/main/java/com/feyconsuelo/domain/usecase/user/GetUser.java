package com.feyconsuelo.domain.usecase.user;

import com.feyconsuelo.domain.model.user.UserResponse;

import java.util.Optional;

public interface GetUser {

    Optional<UserResponse> execute(String username);

}
