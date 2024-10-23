package com.feyconsuelo.domain.usecase.user;

import com.feyconsuelo.domain.model.user.ResetUserPasswordRequest;

public interface ResetUserPassword {

    void execute(ResetUserPasswordRequest resetUserPasswordRequest);

}
