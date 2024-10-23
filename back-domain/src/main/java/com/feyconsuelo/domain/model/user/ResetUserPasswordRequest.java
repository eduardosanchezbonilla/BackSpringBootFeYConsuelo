package com.feyconsuelo.domain.model.user;

import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class ResetUserPasswordRequest {

    String username;
    String password;
    String repeatPassword;

}
