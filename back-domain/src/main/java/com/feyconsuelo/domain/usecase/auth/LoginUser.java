package com.feyconsuelo.domain.usecase.auth;

import com.feyconsuelo.domain.model.auth.AuthRequest;
import com.feyconsuelo.domain.model.auth.AuthResponse;

public interface LoginUser {

    AuthResponse execute(AuthRequest authRequest);

}
