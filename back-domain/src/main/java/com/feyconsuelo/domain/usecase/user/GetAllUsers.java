package com.feyconsuelo.domain.usecase.user;

import com.feyconsuelo.domain.model.user.UserResponse;

import java.util.List;

public interface GetAllUsers {

    List<UserResponse> execute();

}
