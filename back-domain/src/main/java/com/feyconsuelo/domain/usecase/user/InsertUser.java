package com.feyconsuelo.domain.usecase.user;

import com.feyconsuelo.domain.model.user.UserRequest;

public interface InsertUser {

    void execute(UserRequest userRequest);

}
