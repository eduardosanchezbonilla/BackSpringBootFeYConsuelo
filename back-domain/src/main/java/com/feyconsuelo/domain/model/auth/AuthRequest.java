package com.feyconsuelo.domain.model.auth;

import lombok.Builder;
import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@Builder
@EqualsAndHashCode
public class AuthRequest {

    String username;
    String password;

}
