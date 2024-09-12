package com.feyconsuelo.domain.usecase.user;

import com.feyconsuelo.domain.model.user.UpdateUserPasswordRequest;

public interface UpdateUserPassword {

    void execute(UpdateUserPasswordRequest updateUserPasswordRequest);

}
